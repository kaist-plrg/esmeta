package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}
import esmeta.wji.lang.util.Walker
import scala.collection.mutable.ListBuffer

/** ANF-style normalization: extracts every `AlgoCall`-with-args that appears in
  * a non-trivial expression position into a preceding `Let` binding.
  *
  * After this pass every `AlgoCall(f, args)` (args.nonEmpty) is either directly
  * the RHS of a `Let` or `Return` (handled by [[ExtractInlineAlgoCallPass]]) or
  * fully eliminated by substitution.
  *
  * Example:
  * {{{
  *   Append(AlgoCall(f, args), coll)
  *   →  Let(_callN, AlgoCall(f, args))
  *       Append(Var("_callN"), coll)
  * }}}
  * The same hoist applies at any other non-trivial expression position
  * (`IfChain` conditions, `Set` RHS, etc.).
  *
  * [[Instr.Let]] RHS and [[Instr.Return]] value are intentionally skipped at
  * the top level — [[ExtractInlineAlgoCallPass]] converts those to `Perform`
  * directly without an intermediate variable.
  *
  * `!`-wrapped calls (`Abrupt("!", AlgoCall(…))`) are stripped of the wrapper
  * before extraction.
  *
  * For [[Cond.And]] / [[Cond.Or]], only the left (unconditionally-evaluated)
  * side is normalized to preserve short-circuit semantics.
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
  * Category: Structural desugaring.
  */
object NormalizeAlgoCallPass extends LoweringPass:

  /** Requires:
    *   - [[ResolveLinksPass]]: every case here matches `Expr.AlgoCall`/
    *     `Expr.JSCall` specifically — a raw `Expr.Link` falls through the
    *     catch-all cases untouched, so nothing gets extracted at all.
    */
  override def requires: Set[LoweringPass] = Set(ResolveLinksPass)

  private var counter = 0
  private def fresh(): String = { counter += 1; s"_call$counter" }

  /** Extracts every non-trivial `AlgoCall`/`JSCall` reachable from an `Expr`
    * into `hoisted`, substituting a fresh `Var` in its place — accumulates as a
    * side effect during the walk (same trick `esmeta.ir.util.YetCollector` uses
    * for its own mutable buffer), so a single call to `walk`/`extract` both
    * transforms the expression and collects everything that needed hoisting out
    * of it. Only overrides the four call-shaped cases
    * ([[Expr.AlgoCall]]/[[Expr.JSCall]], and their `!`-wrapped forms); every
    * other `Expr` (`BinOp`/`Pow`/`Neg`/`AsMath`/`Length`/`Abrupt`/`Field`/
    * `Index`/`List_`/`Tuple`/`Map_`/[[Expr.Case]] — whose own args may still
    * hide a call, but the `Case` itself is a constructor/pattern, never itself
    * hoisted — and so on) is reached by [[Walker]]'s own exhaustive default
    * recursion.
    */
  private class Extractor extends Walker:
    private val buf = ListBuffer.empty[Instr.Let]
    def hoisted: List[Instr.Let] = buf.toList

    override def walk(expr: Expr): Expr = expr match
      case Expr.AlgoCall(link, args) if args.nonEmpty =>
        // hoist any call nested in this call's own args first (e.g.
        // `F(G(...))`) — otherwise G survives, unextracted, as a
        // nonempty-arg AlgoCall inside the Let this produces, which
        // compileExpr can't handle either.
        val newArgs = args.map(walk)
        val tmp = fresh()
        buf += Instr.Let(Expr.Var(tmp), Expr.AlgoCall(link, newArgs))
        Expr.Var(tmp)
      case Expr.Abrupt("!", Expr.AlgoCall(link, args)) if args.nonEmpty =>
        val newArgs = args.map(walk)
        val tmp = fresh()
        buf += Instr.Let(Expr.Var(tmp), Expr.AlgoCall(link, newArgs))
        Expr.Var(tmp)
      // unlike AlgoCall, JSCall's `[$name$](...)` syntax always carries a
      // (possibly empty) argument list, so there is no zero-arg/bare-reference
      // ambiguity to preserve — every JSCall is a call.
      case Expr.JSCall(name, args) =>
        val newArgs = args.map(walk)
        val tmp = fresh()
        buf += Instr.Let(Expr.Var(tmp), Expr.JSCall(name, newArgs))
        Expr.Var(tmp)
      case Expr.Abrupt("!", Expr.JSCall(name, args)) =>
        val newArgs = args.map(walk)
        val tmp = fresh()
        buf += Instr.Let(Expr.Var(tmp), Expr.JSCall(name, newArgs))
        Expr.Var(tmp)
      case other => super.walk(other)

    def extract(expr: Expr): (List[Instr.Let], Expr) =
      val e = walk(expr)
      (hoisted, e)

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map { a =>
      counter = 0
      a.copy(body = transform(a.body))
    }

  // ── Instr traversal ──────────────────────────────────────────────────────────

  private def transform(instrs: List[Instr]): List[Instr] =
    instrs.flatMap(normalizeInstr)

  private def normalizeInstr(instr: Instr): List[Instr] = instr match

    case Instr.Let(lhs, rhs, body) =>
      val (bindings, newRhs) = skipTopAlgoCall(rhs)
      bindings ++ List(Instr.Let(lhs, newRhs, transform(body)))

    case Instr.Return(Some(rhs), body) =>
      val (bindings, newRhs) = skipTopAlgoCall(rhs)
      bindings ++ List(Instr.Return(Some(newRhs), transform(body)))

    case Instr.Set(lhs, rhs, body) =>
      val (bindings, newRhs) = Extractor().extract(rhs)
      bindings ++ List(Instr.Set(lhs, newRhs, transform(body)))

    case Instr.Append(item, coll, body) =>
      val ext = Extractor()
      val ie = ext.walk(item)
      val ce = ext.walk(coll)
      ext.hoisted ++ List(Instr.Append(ie, ce, transform(body)))

    case Instr.Perform(func, args, outcome, body) =>
      val ext = Extractor()
      val newArgs = args.map(ext.walk)
      ext.hoisted ++ List(
        Instr.Perform(func, newArgs, outcome, transform(body)),
      )

    case Instr.Assert(cond, body) =>
      val (bindings, newCond) = extractFromCond(cond)
      bindings ++ List(Instr.Assert(newCond, transform(body)))

    case Instr.IfChain(branches, fallback) =>
      branches match
        case (cond, body) :: rest =>
          val (bindings, newCond) = extractFromCond(cond)
          val newChain = Instr.IfChain(
            (newCond, transform(body)) :: rest.map((c, b) => (c, transform(b))),
            transform(fallback),
          )
          bindings ++ List(newChain)
        case Nil =>
          List(Instr.IfChain(Nil, transform(fallback)))

    case Instr.While(cond, body) =>
      List(Instr.While(cond, transform(body)))

    case _ =>
      List(instr.mapBody(transform))

  /** Like [[Extractor.extract]] but returns `(Nil, expr)`-shaped results (its
    * own args still extracted) when the top-level expression is already a bare
    * AlgoCall/JSCall (or `!`-wrapped) — those are left for
    * [[ExtractInlineAlgoCallPass]] to convert directly to `Perform` without an
    * intermediate variable.
    */
  private def skipTopAlgoCall(expr: Expr): (List[Instr.Let], Expr) = expr match
    // the call itself is left in place for ExtractInlineAlgoCallPass to
    // convert directly to Perform, but its own args can still hide a nested
    // call (e.g. `Let x be F(G(...))`) that needs hoisting first.
    case Expr.AlgoCall(link, args) =>
      val ext = Extractor()
      val newArgs = args.map(ext.walk)
      (ext.hoisted, Expr.AlgoCall(link, newArgs))
    case Expr.Abrupt("!", Expr.AlgoCall(link, args)) =>
      val ext = Extractor()
      val newArgs = args.map(ext.walk)
      (ext.hoisted, Expr.Abrupt("!", Expr.AlgoCall(link, newArgs)))
    case Expr.JSCall(name, args) =>
      val ext = Extractor()
      val newArgs = args.map(ext.walk)
      (ext.hoisted, Expr.JSCall(name, newArgs))
    case Expr.Abrupt("!", Expr.JSCall(name, args)) =>
      val ext = Extractor()
      val newArgs = args.map(ext.walk)
      (ext.hoisted, Expr.Abrupt("!", Expr.JSCall(name, newArgs)))
    case _ => Extractor().extract(expr)

  // ── Cond normalization ───────────────────────────────────────────────────────

  private def extractFromCond(cond: Cond): (List[Instr.Let], Cond) = cond match

    case Cond.Eq(l, r, neg) =>
      val ext = Extractor()
      val (le, re) = (ext.walk(l), ext.walk(r))
      (ext.hoisted, Cond.Eq(le, re, neg))

    case Cond.Compare(l, op, r) =>
      val ext = Extractor()
      val (le, re) = (ext.walk(l), ext.walk(r))
      (ext.hoisted, Cond.Compare(le, op, re))

    case Cond.IsType(e, t, neg) =>
      val (b, ne) = Extractor().extract(e); (b, Cond.IsType(ne, t, neg))

    case Cond.HasField(e, neg) =>
      val (b, ne) = Extractor().extract(e); (b, Cond.HasField(ne, neg))

    case Cond.Implements(e, iface, neg) =>
      val (b, ne) = Extractor().extract(e); (b, Cond.Implements(ne, iface, neg))

    case Cond.IsMissing(e, neg) =>
      val (b, ne) = Extractor().extract(e); (b, Cond.IsMissing(ne, neg))

    case Cond.IsOfForm(e, form, condOpt, neg) =>
      val (b, ne) = Extractor().extract(e)
      (b, Cond.IsOfForm(ne, form, condOpt, neg))

    case Cond.And(l, r) =>
      val (lb, lc) = extractFromCond(l)
      (lb, Cond.And(lc, r))

    case Cond.Or(l, r) =>
      val (lb, lc) = extractFromCond(l)
      (lb, Cond.Or(lc, r))

    // `collections` is evaluated once, before ExpandMatchesExistsPass's
    // generated loop even starts, so hoisting a call out of it here is safe
    // — unlike `body`, which runs once per iteration and references the
    // loop-bound `binder`; extracting from `body` at this stage (before that
    // loop exists) would evaluate it in the wrong scope, so it's deliberately
    // left alone for ExpandMatchesExistsPass's own (loop-aware) handling.
    case Cond.Exists(binder, collections, body) =>
      val ext = Extractor()
      val es = collections.map(ext.walk)
      (ext.hoisted, Cond.Exists(binder, es, body))

    case other => (Nil, other)
