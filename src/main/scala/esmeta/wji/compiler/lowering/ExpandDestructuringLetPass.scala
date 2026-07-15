package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Expr, Instr}

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
  * destructured fields are built as [[Expr.TupleProj]] (compiles to
  * `ir.EProj`), not a plain [[Expr.Index]] (compiles to a heap `Field` read;
  * see `esmeta.wji.compiler.Compiler`).
  *
  * `Case(tag, args)` as a `Let` LHS (e.g. "Let [|parameters|] → [|results|] be
  * |functype|.", parsed by `ExprParser.CompTypeArrow` into `Case("->",
  * [Var(parameters), Var(results)])`) destructures the very same way — the
  * runtime value is a `Wasm(CaseV(tag, args))` either way, `Case` only
  * additionally names *which* variant it's shaped as (unchecked here, same as
  * this pass already trusts a tuple-shaped `Let`'s RHS without asserting its
  * arity). Only fires when every arg is a bare `Expr.Var`, mirroring
  * `ExpandIsOfFormPass`'s same guard on the condition side.
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
  */
object ExpandDestructuringLetPass extends LoweringPass:
  private var counter = 0
  private def freshTuple(): String = { counter += 1; s"_tuple$counter" }

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = transform(a.body)))

  private def transform(instrs: List[Instr]): List[Instr] =
    instrs.flatMap(expandInstr)

  private def destructure(
    elems: List[Expr],
    expr: Expr,
    body: List[Instr],
  ): List[Instr] =
    val (base, binding) = expr match
      case v: Expr.Var => (v, Nil)
      case _ =>
        val tmp = Expr.Var(freshTuple())
        (tmp, List(Instr.Let(tmp, expr)))
    val destructures = elems.zipWithIndex.map { (elem, i) =>
      Instr.Let(elem, Expr.TupleProj(base, i))
    }
    binding ++ destructures ++ transform(body)

  private def expandInstr(instr: Instr): List[Instr] = instr match
    case Instr.Let(Expr.Tuple(elems), expr, body) =>
      destructure(elems, expr, body)
    case Instr.Let(Expr.Case(_, args), expr, body)
        if args.forall(_.isInstanceOf[Expr.Var]) =>
      destructure(args, expr, body)
    case _ =>
      List(instr.mapBody(transform))
