package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}
import esmeta.error.UnsupportedSpecShape

/** Expands `Let(Tuple([x, y, ...]), expr, body)` and `Let(Case(tag, [x, y,
  * ...]), expr, body)` into individual bindings.
  *
  * {{{
  *   Let((|store|, |addr|), call)
  * }}}
  * becomes:
  * {{{
  *   Let(_tupleN, call)
  *   Let(|store|, _tupleN.0)
  *   Let(|addr|,  _tupleN.1)
  * }}}
  *
  * Every tuple-destructuring `Let` in the spec (directly or indirectly, via a
  * preceding `Let |result| be ...`) unpacks the `(store, X)` result of a Wasm
  * embedding call — a `Wasm(TupV(...))`, not a heap list/record — so the
  * destructured fields are built as [[Expr.TupleProj]] rather than a plain
  * [[Expr.Index]]: both compile to the same `ir.Field` ref (see
  * `esmeta.wji.compiler.Compiler`, `State.apply` dispatches on the base value's
  * actual runtime shape either way), but `TupleProj`'s index is always a
  * literal `Int` known at lowering time, not an arbitrary key `Expr`.
  *
  * `Case(tag, args)` as a `Let` LHS (e.g. "Let [=comp-type/func=] |parameters|
  * → |results| be |functype|.", parsed by `ExprParser`'s comptype-arrow case
  * into `Case("[=comp-type/func=]", [Var(parameters), Var(results)])`)
  * destructures the very same way — the runtime value is a `Wasm(CaseV(tag,
  * args))` either way, `Case` only additionally names *which* variant it's
  * shaped as. Unlike the plain-tuple case, this variant *does* assert the tag
  * first (`Assert(Eq(CaseTag(base), Str(tag)))`) before projecting: `functype`
  * genuinely is a `deftype`, not always already the expected `->`-shaped
  * comptype (see `docs/spec_errors.md` on `$expand`), and positionally
  * projecting the wrong-shaped value without checking silently hands the caller
  * garbage (e.g. `_DEF`'s own inner `rectype`/index in place of a
  * params/results list) that only surfaces as a confusing failure several
  * instructions later, rather than right here where the actual mismatch is.
  * Fires when every arg is either a bare `Expr.Var` or a literal empty
  * `Expr.List_(Nil)` (a `« »` side, e.g. "Let [=comp-type/func=] |types| → « »
  * be ..." — a functype with no results) — the latter isn't bound to anything
  * (see `destructure`'s `collect` below, which only emits a projection for the
  * `Var` elements, silently skipping any `List_(Nil)` position while still
  * using its correct index for the elements after it); strictly, `« »` means
  * more than "don't bind this side", it also asserts the runtime list at that
  * position really is empty, which isn't checked here — left for whenever that
  * distinction actually matters. Mirrors `ExpandIsOfFormPass`'s same guard on
  * the condition side — a `Case`-lhs `Let` with any other kind of arg throws
  * `UnsupportedSpecShape` instead of silently reaching `Compiler`'s much later,
  * less specific `EYet` fallback. `tag` itself needs no translation here:
  * `NormalizeSpecTecCaseShapePass` (which every `Case` this pass sees has
  * already been through, `requires` below) already normalized it into the real
  * runtime tag, whichever way it was originally spelled — a bare runtime tag to
  * begin with, spec-link text (`ResolveLinksPass`'s `[=external-type/func=]`
  * shapes, e.g. `create_an_exports_object`'s `externval` destructuring, or
  * `NormalizeSpecTecCaseShapePass`'s own raw-tag special case for
  * `"[=comp-type/func=]"`), or the empty tag (`ExprParser.UntaggedMultiVar`'s
  * "Let |mut| |valuetype| be ..." shape, no keyword between the components —
  * needs no special-casing either, the assert just compares against `""` like
  * any other tag, and that's exactly what a value shaped this way actually
  * carries at runtime: `construct.ml`'s `al_of_globaltype`, `GlobalT (mut, vt)
  * -> CaseV ("", [al_of_mut mut; al_of_valtype vt])`).
  *
  * The original body (if non-empty) is appended after the destructured
  * bindings. In practice, tuple-destructuring Let nodes in the spec have empty
  * bodies.
  *
  * When `expr` is already a bare [[Expr.Var]] — e.g. "Let [=comp-type/func=]
  * |parameters| → |results| be |functype|.", where the RHS is just a variable
  * reference — the temp binding is skipped and the projections read straight
  * from it: a plain variable reference can't have a side effect worth
  * deduplicating (the temp only exists so an effectful RHS, e.g. an embedding
  * call like `tag_alloc`, is evaluated once and shared, not once per projected
  * field).
  *
  * Open question, not yet confirmed either way: the `Case`-lhs branch below may
  * also need [[ResolveLinksPass]] if a Link-derived `Expr.Case` (as opposed to
  * `ExprParser`'s directly-parsed comptype-arrow tag) ever shows up as a
  * destructuring target — not added to `requires` until that's actually
  * confirmed.
  *
  * Category: Spec-dependent — SpecTec.
  */
object ExpandDestructuringLetPass extends LoweringPass:

  /** Requires:
    *   - [[ExpandForEachPass]]: destructures the `Let(Tuple(elems), ...)` that
    *     pass produces for a `Tuple`-shaped `ForEach` element — this pass is
    *     the one that unpacks it into individual bindings.
    *   - [[NormalizeSpecTecCaseShapePass]]: a `Case`-lhs `Let`'s `tag` must
    *     already be the real runtime tag by the time `destructure` asserts
    *     against it — this pass does no translation of its own.
    */
  override def requires: Set[LoweringPass] =
    Set(ExpandForEachPass, NormalizeSpecTecCaseShapePass)

  /** Generates this pass's `_tupleN` names for a single algorithm. Scoped as a
    * value local to each [[run]] iteration rather than a mutable field on this
    * `object` — the latter is JVM-wide singleton state, so concurrent `run`
    * calls (e.g. multiple ScalaTest suites compiling algorithms in parallel,
    * which sbt's default `Test / parallelExecution` allows) would race on
    * incrementing/resetting a shared counter, producing nondeterministic naming
    * depending on thread interleaving.
    */
  private class Counter:
    private var n = 0
    def freshTuple(): String = { n += 1; s"_tuple$n" }

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map { a =>
      val counter = Counter()
      a.copy(body = transform(a.body, counter))
    }

  private def transform(instrs: List[Instr], counter: Counter): List[Instr] =
    instrs.flatMap(expandInstr(_, counter))

  private def destructure(
    elems: List[Expr],
    expr: Expr,
    body: List[Instr],
    counter: Counter,
    tagCheck: Option[String] = None,
  ): List[Instr] =
    val (base, binding) = expr match
      case v: Expr.Var => (v, Nil)
      case _ =>
        val tmp = Expr.Var(counter.freshTuple())
        (tmp, List(Instr.Let(tmp, expr)))
    val assertTag = tagCheck.toList.map { tag =>
      Instr.Assert(Cond.Eq(Expr.CaseTag(base), Expr.Str(tag)))
    }
    val destructures = elems.zipWithIndex.collect {
      case (v: Expr.Var, i) => Instr.Let(v, Expr.TupleProj(base, i))
    }
    binding ++ assertTag ++ destructures ++ transform(body, counter)

  private def expandInstr(instr: Instr, counter: Counter): List[Instr] =
    instr match
      case Instr.Let(Expr.Tuple(elems), expr, body) =>
        destructure(elems, expr, body, counter)
      case Instr.Let(Expr.Case(tag, args), expr, body)
          if args.forall(a => a.isInstanceOf[Expr.Var] || a == Expr.List_(Nil),
          ) =>
        destructure(args, expr, body, counter, tagCheck = Some(tag))
      case Instr.Let(lhs @ Expr.Case(_, _), _, _) =>
        throw UnsupportedSpecShape(
          "ExpandDestructuringLetPass",
          s"Case-lhs Let with an arg that's neither a Var nor a literal empty list: $lhs",
        )
      case _ =>
        List(instr.mapBody(transform(_, counter)))
