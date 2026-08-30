package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}
import esmeta.wji.lang.walker.Walker
import scala.collection.mutable.ListBuffer

/** ANF-style normalization implementing ECMA-262's own "Evaluation Order"
  * rewrite (see 5.2.4.3, Shorthands for Unwrapping Completion Records): hoists
  * every non-trivial `AlgoCall`/`JSCall` *and* every `Expr.Abrupt` (`?`/`!`)
  * that appears in a non-trivial expression position into a preceding `Let`
  * binding. The spec text treats both as the same rewrite — "When `?` or `!` is
  * used in any other context, first apply the rewrite given in Evaluation Order
  * until this rule can be applied, then apply this rule" — so both are handled
  * by the same [[Extractor]] walk here, not two separate ones: a call nested
  * inside a `?`, or a `?` nested inside a call's argument, are reached
  * correctly either way because there's only one recursive walk that knows
  * about both, not two that might each be blind to the other's job.
  *
  * After this pass every `AlgoCall(f, args)` (args.nonEmpty) is either directly
  * the RHS of a `Let`/`Return` (handled by [[ExpandInlineAlgoCallPass]]) or
  * fully eliminated by substitution; every `Expr.Abrupt(marker, inner)` has
  * `inner` already a bare `Var`, and is itself either directly the RHS of a
  * `Let`/`Set`/`Return` (handled by [[ExpandAbruptPass]], which now only ever
  * needs to match that one canonical shape) or fully eliminated by
  * substitution.
  *
  * Example:
  * {{{
  *   Append(AlgoCall(f, args), coll)
  *   →  Let(_callN, AlgoCall(f, args))
  *       Append(Var("_callN"), coll)
  * }}}
  * and
  * {{{
  *   Append(Abrupt("?", AlgoCall(f, args)), coll)
  *   →  Let(_callN, AlgoCall(f, args))
  *       Let(_callM, Abrupt("?", Var("_callN")))
  *       Append(Var("_callM"), coll)
  * }}}
  * The same hoist applies at any other non-trivial expression position
  * (`IfChain` conditions, `Set` RHS, etc.).
  *
  * [[Instr.Let]]/[[Instr.Return]]'s direct RHS is special-cased via
  * [[Extractor.walkTop]]: a bare `AlgoCall`/`JSCall` is left in place (that
  * instruction already *is* a binding slot — [[ExpandInlineAlgoCallPass]]
  * converts it to `Perform` directly, without an intermediate variable), and an
  * `Expr.Abrupt` is left in place too, once its own `inner` is a bare `Var`
  * (same reasoning — the enclosing `Let`/`Return` is already the binding
  * [[ExpandAbruptPass]] needs, so wrapping it in yet another fresh alias would
  * only add a redundant indirection). `Instr.Set` gets the same treatment, but
  * *only* when its RHS is `Expr.Abrupt` — a bare call there still gets fully
  * hoisted (there's no `Perform` outcome that assigns into an existing variable
  * the way `BindResult`/`ReturnResult` do for `Let`/`Return`, so `Set(x, call)`
  * always becomes `Let(_callN, call); Set(x, Var(_callN))`).
  *
  * For [[Cond.And]] / [[Cond.Or]], the left (unconditionally-evaluated) side is
  * flat-hoisted like anywhere else, to preserve short-circuit semantics. A call
  * needed by the right side can't be flat-hoisted the same way (it must run
  * only once the left side's value is known) — `extractFromCond` instead lowers
  * the whole `And`/`Or` into a `Let`+guard-`IfChain` preamble that computes a
  * fresh boolean flag (same "declare false, conditionally set true" shape
  * `ExpandMatchesExistsPass` already produces for `Cond.Any`'s `_found` flag),
  * replacing the original condition with a plain equality check on that flag —
  * so the caller's own `thenBody`/`elseBody` still appear exactly once each,
  * never duplicated by the nesting.
  *
  * [[Instr.While]] conditions are never extracted (they are re-evaluated each
  * iteration; extraction would change semantics).
  *
  * `extractFromCond` deliberately does *not* recurse into every `Cond` variant
  * — only the ones spec prose has actually needed extraction inside so far
  * (`Eq`/`Compare`/`IsType`/`HasField`/`Implements`/`IsMissing`, plus
  * `IsOfForm`'s `expr` but never its `form`, and `And`/`Or`/`Exists`'s
  * position-sensitive halves below); `Matches`/`HasSlot`/`HasDuplicates`/
  * `Contains`/`Abbreviated`/etc. pass through untouched, same as always — this
  * narrower-than-generic scope is why `extractFromCond` stays its own
  * hand-written function rather than a [[Walker]] override (a `Walker`'s
  * default `walk(Cond)` recurses into every variant, which would extract calls
  * from cases this pass has never touched).
  *
  * Category: Structural desugaring — Reordering.
  */
object NormalizeEvaluationOrderPass extends LoweringPass:

  /** Requires:
    *   - [[ResolveLinksPass]]: every case here matches `Expr.AlgoCall`/
    *     `Expr.JSCall` specifically — a raw `Expr.Link` falls through the
    *     catch-all cases untouched, so nothing gets extracted at all.
    */
  override def requires: Set[LoweringPass] = Set(ResolveLinksPass)

  /** Generates this pass's `_callN` names for a single algorithm. Scoped as a
    * value local to each [[run]] iteration (see below) rather than a mutable
    * field on this `object` — the latter is JVM-wide singleton state, so
    * concurrent `run` calls (e.g. multiple ScalaTest suites compiling
    * algorithms in parallel, which sbt's default `Test / parallelExecution`
    * allows) would race on incrementing/resetting a shared counter, producing
    * nondeterministic `_callN` numbering depending on thread interleaving.
    */
  private class Counter:
    private var n = 0
    def fresh(): String = { n += 1; s"_call$n" }

    /** Names for the boolean flag `extractFromCond`'s `Cond.And`/`Cond.Or`
      * cases synthesize — sharing `n` with [[fresh]] (distinct prefix is enough
      * to avoid collisions) rather than a separate counter, since both only
      * ever need to be unique, never contiguous.
      */
    def freshCond(): String = { n += 1; s"_cond$n" }

  /** Extracts every non-trivial `AlgoCall`/`JSCall`/`Expr.Abrupt` reachable
    * from an `Expr` into `hoisted`, substituting a fresh `Var` in its place —
    * accumulates as a side effect during the walk (same trick
    * `esmeta.ir.util.YetCollector` uses for its own mutable buffer), so a
    * single call to `walk`/`extract` both transforms the expression and
    * collects everything that needed hoisting out of it. Only overrides the
    * three hoistable-shape cases — e.g. [[Expr.Case]]'s own args may still hide
    * a call or `?`/`!`, but the `Case` itself is a constructor/pattern, never
    * itself hoisted.
    */
  private class Extractor(counter: Counter) extends Walker:
    private val buf = ListBuffer.empty[Instr.Let]
    def hoisted: List[Instr.Let] = buf.toList

    /** Binds `e` to a fresh `Let`, appended to `buf`, and returns a `Var`
      * referencing it.
      */
    def bindFresh(e: Expr): Expr.Var =
      val tmp = counter.fresh()
      buf += Instr.Let(Expr.Var(tmp), e)
      Expr.Var(tmp)

    /** Ensures `e` is a bare `Var`, hoisting it into a fresh `Let` first if
      * it's anything else — mirrors mainline `esmeta.compiler.Compiler.
      * returnIfAbrupt`'s own `case ERef(local: Local) => (local, expr)`: if `e`
      * is already a plain variable reference, reuse it directly rather than
      * rebinding it under a redundant fresh alias.
      */
    def ensureVar(e: Expr): Expr.Var = e match
      case v: Expr.Var => v
      case other       => bindFresh(other)

    override def walk(expr: Expr): Expr = expr match
      case Expr.AlgoCall(link, args) =>
        // hoist any call nested in this call's own args first (e.g.
        // `F(G(...))`) — otherwise G survives, unextracted, as a
        // nonempty-arg AlgoCall inside the Let this produces, which
        // compileExpr can't handle either.
        bindFresh(Expr.AlgoCall(link, args.map(walk)))
      // unlike AlgoCall, JSCall's `[$name$](...)` syntax always carries a
      // (possibly empty) argument list, so there is no zero-arg/bare-reference
      // ambiguity to preserve — every JSCall is a call.
      case Expr.JSCall(name, args) =>
        bindFresh(Expr.JSCall(name, args.map(walk)))
      case Expr.Abrupt(marker, inner) =>
        // ECMA-262's own two-step "Evaluation Order" rewrite: first ensure
        // the wrapped value is itself a bare Var (hoisting any call still
        // inside it), then hoist the `?`/`!` application itself — after
        // this, `ExpandAbruptPass` only ever needs to recognize `Abrupt(
        // marker, Var(_))` as a direct Let RHS, nothing else.
        bindFresh(Expr.Abrupt(marker, ensureVar(walk(inner))))
      case other => super.walk(other)

    def extract(expr: Expr): (List[Instr.Let], Expr) =
      val e = walk(expr)
      (hoisted, e)

    /** Like [[walk]] but leaves `expr` itself un-hoisted when it's a bare
      * `AlgoCall`/`JSCall` (for [[ExpandInlineAlgoCallPass]] to convert
      * directly) or an `Expr.Abrupt` whose `inner` is already atomic (for
      * [[ExpandAbruptPass]] to unwrap directly) — see class doc. Used only at a
      * `Let`/`Return`'s own direct-RHS position, where the enclosing
      * instruction already provides the binding slot either would otherwise
      * need a fresh one for.
      */
    def walkTop(expr: Expr): Expr = expr match
      case Expr.AlgoCall(link, args) => Expr.AlgoCall(link, args.map(walk))
      case Expr.JSCall(name, args)   => Expr.JSCall(name, args.map(walk))
      case Expr.Abrupt(marker, inner) =>
        Expr.Abrupt(marker, ensureVar(walk(inner)))
      case other => walk(other)

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map { a =>
      val counter = Counter()
      a.copy(body = transform(a.body, counter))
    }

  // ── Instr traversal ──────────────────────────────────────────────────────────

  private def transform(instrs: List[Instr], counter: Counter): List[Instr] =
    instrs.flatMap(normalizeInstr(_, counter))

  private def normalizeInstr(instr: Instr, counter: Counter): List[Instr] =
    instr match

      case Instr.Let(lhs, rhs, body) =>
        val ext = Extractor(counter)
        val newRhs = ext.walkTop(rhs)
        ext.hoisted ++ List(Instr.Let(lhs, newRhs, transform(body, counter)))

      case Instr.Return(Some(rhs), body) =>
        val ext = Extractor(counter)
        val newRhs = ext.walkTop(rhs)
        ext.hoisted ++ List(
          Instr.Return(Some(newRhs), transform(body, counter)),
        )

      // `Set`'s RHS has no `walkTop`-style exemption for a bare call (see class
      // doc — no `Perform` outcome assigns into an existing variable), but an
      // `Expr.Abrupt` RHS still gets the same "leave at top, just ensure
      // `inner` is atomic" treatment as `Let`/`Return`: `lhs` is already
      // `ExpandAbruptPass`'s binding slot, so hoisting the whole `Abrupt` into
      // yet another fresh alias would just be redundant.
      case Instr.Set(lhs, Expr.Abrupt(marker, inner), body) =>
        val ext = Extractor(counter)
        val newRhs = Expr.Abrupt(marker, ext.ensureVar(ext.walk(inner)))
        ext.hoisted ++ List(Instr.Set(lhs, newRhs, transform(body, counter)))

      case Instr.Set(lhs, rhs, body) =>
        val (bindings, newRhs) = Extractor(counter).extract(rhs)
        bindings ++ List(Instr.Set(lhs, newRhs, transform(body, counter)))

      case Instr.Append(item, coll, body) =>
        val ext = Extractor(counter)
        val ie = ext.walk(item)
        val ce = ext.walk(coll)
        ext.hoisted ++ List(Instr.Append(ie, ce, transform(body, counter)))

      case Instr.Throw(target, body) =>
        val ext = Extractor(counter)
        val newTarget = ext.walk(target)
        ext.hoisted ++ List(Instr.Throw(newTarget, transform(body, counter)))

      case Instr.Perform(func, args, outcome, body) =>
        val ext = Extractor(counter)
        val newArgs = args.map(ext.walk)
        ext.hoisted ++ List(
          Instr.Perform(func, newArgs, outcome, transform(body, counter)),
        )

      case Instr.Assert(cond, body) =>
        val (bindings, newCond) = extractFromCond(cond, counter)
        bindings ++ List(Instr.Assert(newCond, transform(body, counter)))

      case Instr.IfChain(branches, fallback) =>
        buildChain(branches, fallback, counter)

      case Instr.While(cond, body) =>
        List(Instr.While(cond, transform(body, counter)))

      case _ =>
        List(instr.mapBody(transform(_, counter)))

  /** Rebuilds an `IfChain`'s branch list back-to-front, hoisting each branch's
    * own condition via [[extractFromCond]] as it goes. A branch whose condition
    * needs nothing hoisted is folded back in as a flat sibling of whatever
    * `rest` already built (so a chain that needs no hoisting anywhere
    * round-trips through this function with its original flat shape completely
    * unchanged — confirmed by `SnapshotSpec` showing zero diff outside the one
    * algorithm this fix actually targets). Once a branch *does* need hoisting,
    * flattening can't continue past it: that branch's `Let`s only belong where
    * they're now placed — right before a `IfChain` wrapping *just* that branch,
    * itself reachable only once every earlier branch's condition has already
    * been false — so everything from there on nests one level deeper instead of
    * staying a sibling. This is the fix for the bug the flat version had:
    * hoisting only the *first* branch's condition and leaving every later
    * `ElseIf`'s condition untouched, silently leaving a call embedded raw
    * inside it (see `docs/hardcodes.md`/this pass's own class doc — a later
    * branch's condition is only ever evaluated once earlier ones are known
    * false, so its hoisted `Let`s can't just be flattened unconditionally to
    * the top the way the first branch's already correctly are).
    */
  private def buildChain(
    branches: List[(Cond, List[Instr])],
    fallback: List[Instr],
    counter: Counter,
  ): List[Instr] = branches match
    case Nil => transform(fallback, counter)
    case (cond, body) :: rest =>
      val (bindings, newCond) = extractFromCond(cond, counter)
      val newBody = transform(body, counter)
      val restInstrs = buildChain(rest, fallback, counter)
      if bindings.isEmpty then
        restInstrs match
          case List(Instr.IfChain(restBranches, restFallback)) =>
            List(
              Instr.IfChain((newCond, newBody) :: restBranches, restFallback),
            )
          case other =>
            List(Instr.IfChain(List((newCond, newBody)), other))
      else bindings ++ List(Instr.IfChain(List((newCond, newBody)), restInstrs))

  // ── Cond normalization ───────────────────────────────────────────────────────

  private def extractFromCond(
    cond: Cond,
    counter: Counter,
  ): (List[Instr], Cond) = cond match

    case Cond.Eq(l, r, neg) =>
      val ext = Extractor(counter)
      val (le, re) = (ext.walk(l), ext.walk(r))
      (ext.hoisted, Cond.Eq(le, re, neg))

    case Cond.Compare(l, op, r) =>
      val ext = Extractor(counter)
      val (le, re) = (ext.walk(l), ext.walk(r))
      (ext.hoisted, Cond.Compare(le, op, re))

    case Cond.IsType(e, t, neg) =>
      val (b, ne) = Extractor(counter).extract(e); (b, Cond.IsType(ne, t, neg))

    case Cond.HasField(e, neg) =>
      val (b, ne) = Extractor(counter).extract(e); (b, Cond.HasField(ne, neg))

    case Cond.Implements(e, iface, neg) =>
      val (b, ne) = Extractor(counter).extract(e)
      (b, Cond.Implements(ne, iface, neg))

    case Cond.IsMissing(e, neg) =>
      val (b, ne) = Extractor(counter).extract(e); (b, Cond.IsMissing(ne, neg))

    case Cond.IsOfForm(e, form, condOpt, neg) =>
      val (b, ne) = Extractor(counter).extract(e)
      (b, Cond.IsOfForm(ne, form, condOpt, neg))

    // The right operand is only evaluable once the left's value is known, so
    // any call it needs can't be flat-hoisted next to the left's own hoisted
    // `Let`s the way `Eq`/`Compare`/etc.'s two sides are above — hence the
    // recursive `extractFromCond(r, ...)` here (unlike a plain `walk`), and
    // the `Let`/guard-`IfChain` preamble once it comes back non-empty: a
    // fresh flag starts false, gets set true only along the path that
    // matches `And`/`Or`'s own short-circuit rule, and stands in for the
    // whole condition afterward — so the branch this condition guards still
    // ends up as a single flat `Eq` check, never duplicated.
    case Cond.And(l, r) =>
      val (lb, lc) = extractFromCond(l, counter)
      val (rb, rc) = extractFromCond(r, counter)
      if rb.isEmpty then (lb, Cond.And(lc, rc))
      else
        val flag = Expr.Var(counter.freshCond())
        val setIfRight =
          Instr.IfChain(List((rc, List(Instr.Set(flag, Expr.Bool(true))))), Nil)
        val guard =
          Instr.IfChain(List((lc, rb ++ List(setIfRight))), Nil)
        (
          lb ++ List(Instr.Let(flag, Expr.Bool(false)), guard),
          Cond.Eq(flag, Expr.Bool(true)),
        )

    case Cond.Or(l, r) =>
      val (lb, lc) = extractFromCond(l, counter)
      val (rb, rc) = extractFromCond(r, counter)
      if rb.isEmpty then (lb, Cond.Or(lc, rc))
      else
        val flag = Expr.Var(counter.freshCond())
        val setIfRight =
          Instr.IfChain(List((rc, List(Instr.Set(flag, Expr.Bool(true))))), Nil)
        val guard = Instr.IfChain(
          List((lc, List(Instr.Set(flag, Expr.Bool(true))))),
          rb ++ List(setIfRight),
        )
        (
          lb ++ List(Instr.Let(flag, Expr.Bool(false)), guard),
          Cond.Eq(flag, Expr.Bool(true)),
        )

    // `collections` is evaluated once, before ExpandMatchesExistsPass's
    // generated loop even starts, so hoisting a call out of it here is safe
    // — unlike `body`, which runs once per iteration and references the
    // loop-bound `binder`; extracting from `body` at this stage (before that
    // loop exists) would evaluate it in the wrong scope, so it's deliberately
    // left alone for ExpandMatchesExistsPass's own (loop-aware) handling.
    case Cond.Any(binder, collections, body, neg) =>
      val ext = Extractor(counter)
      val es = collections.map(ext.walk)
      (ext.hoisted, Cond.Any(binder, es, body, neg))

    // `Cond.Exists` has no `collections` of its own to hoist from at all
    // (unlike `Any`) — its whole `body` runs conditionally, once per
    // candidate the eventual search loop tries, same reasoning as `Any`'s
    // `body` above — so this falls through to the `case other` default
    // below unchanged, deliberately, rather than needing its own case here.

    case other => (Nil, other)
