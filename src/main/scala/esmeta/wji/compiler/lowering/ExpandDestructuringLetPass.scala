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
  * `Case(tag, args)` as a `Let` LHS (e.g. "Let [|parameters|] → [|results|] be
  * |functype|.", parsed by `ExprParser.CompTypeArrow` into `Case("->",
  * [Var(parameters), Var(results)])`) destructures the very same way — the
  * runtime value is a `Wasm(CaseV(tag, args))` either way, `Case` only
  * additionally names *which* variant it's shaped as. Unlike the plain-tuple
  * case, this variant *does* assert the tag first (`Assert(Eq(CaseTag(base),
  * Str(Expr.runtimeCaseTag(tag))))`) before projecting: `functype` genuinely is
  * a `deftype`, not always already the expected `->`-shaped comptype (see
  * `docs/spec_errors.md` on `$expand`), and positionally projecting the
  * wrong-shaped value without checking silently hands the caller garbage (e.g.
  * `_DEF`'s own inner `rectype`/index in place of a params/results list) that
  * only surfaces as a confusing failure several instructions later, rather than
  * right here where the actual mismatch is. Only fires when every arg is a bare
  * `Expr.Var`, mirroring `ExpandIsOfFormPass`'s same guard on the condition
  * side — a `Case`-lhs `Let` with a non-`Var` arg throws `UnsupportedSpecShape`
  * instead of silently reaching `Compiler`'s much later, less specific `EYet`
  * fallback. `Expr.runtimeCaseTag` (shared with `Compiler`'s own `ECase`
  * construction) translates `tag` the same way either way it's spelled —
  * already a bare runtime tag (`CompTypeArrow`'s literal `"->"`) or spec-link
  * text (`ResolveLinksPass`'s `[=external-type/func=]` shapes, e.g.
  * `create_an_exports_object`'s `externval` destructuring).
  *
  * The original body (if non-empty) is appended after the destructured
  * bindings. In practice, tuple-destructuring Let nodes in the spec have empty
  * bodies.
  *
  * When `expr` is already a bare [[Expr.Var]] — e.g. "Let [|parameters|] →
  * [|results|] be |functype|.", where the RHS is just a variable reference —
  * the temp binding is skipped and the projections read straight from it: a
  * plain variable reference can't have a side effect worth deduplicating (the
  * temp only exists so an effectful RHS, e.g. an embedding call like
  * `tag_alloc`, is evaluated once and shared, not once per projected field).
  *
  * Open question, not yet confirmed either way: the `Case`-lhs branch below may
  * also need [[ResolveLinksPass]] if a Link-derived `Expr.Case` (as opposed to
  * `ExprParser.CompTypeArrow`'s directly-parsed `"->"` tag) ever shows up as a
  * destructuring target — not added to `requires` until that's actually
  * confirmed.
  *
  * Category: Structural desugaring.
  */
object ExpandDestructuringLetPass extends LoweringPass:

  /** Requires:
    *   - [[ExpandForEachPass]]: destructures the `Let(Tuple(elems), ...)` that
    *     pass produces for a `Tuple`-shaped `ForEach` element — this pass is
    *     the one that unpacks it into individual bindings.
    */
  override def requires: Set[LoweringPass] = Set(ExpandForEachPass)

  private var counter = 0
  private def freshTuple(): String = { counter += 1; s"_tuple$counter" }

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map { a =>
      counter = 0
      a.copy(body = transform(a.body))
    }

  private def transform(instrs: List[Instr]): List[Instr] =
    instrs.flatMap(expandInstr)

  private def destructure(
    elems: List[Expr],
    expr: Expr,
    body: List[Instr],
    tagCheck: Option[String] = None,
  ): List[Instr] =
    val (base, binding) = expr match
      case v: Expr.Var => (v, Nil)
      case _ =>
        val tmp = Expr.Var(freshTuple())
        (tmp, List(Instr.Let(tmp, expr)))
    val assertTag = tagCheck.toList.map { tag =>
      Instr.Assert(
        Cond.Eq(Expr.CaseTag(base), Expr.Str(Expr.runtimeCaseTag(tag))),
      )
    }
    val destructures = elems.zipWithIndex.map { (elem, i) =>
      Instr.Let(elem, Expr.TupleProj(base, i))
    }
    binding ++ assertTag ++ destructures ++ transform(body)

  private def expandInstr(instr: Instr): List[Instr] = instr match
    case Instr.Let(Expr.Tuple(elems), expr, body) =>
      destructure(elems, expr, body)
    case Instr.Let(Expr.Case(tag, args), expr, body)
        if args.forall(_.isInstanceOf[Expr.Var]) =>
      destructure(args, expr, body, tagCheck = Some(tag))
    case Instr.Let(lhs @ Expr.Case(_, _), _, _) =>
      throw UnsupportedSpecShape(
        "ExpandDestructuringLetPass",
        s"Case-lhs Let with a non-Var arg: $lhs",
      )
    case _ =>
      List(instr.mapBody(transform))
