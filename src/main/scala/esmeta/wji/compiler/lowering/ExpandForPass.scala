package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}

/** Expands `Instr.For(elem, Range(low, high), body)` into a counting while
  * loop:
  *
  * {{{
  *   For(x, Range(low, high), body)
  * }}}
  * becomes:
  * {{{
  *   Let(x, low)
  *   While(x <= high,
  *     ...body...
  *     Set(x, x + 1))
  * }}}
  *
  * Both bounds are inclusive (see [[esmeta.wji.lang.Expr.Range]]), so `x`
  * itself doubles as the loop counter — unlike [[ExpandForEachPass]], no
  * separate index variable is needed.
  *
  * Only handles a bare `Var` `elem` over a `Range` `collection`. Any other
  * shape is left as `For` for a future pass.
  *
  * Category: Structural desugaring — Elimination.
  */
object ExpandForPass extends LoweringPass:
  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = transform(a.body)))

  private def transform(instrs: List[Instr]): List[Instr] =
    instrs.flatMap(expandInstr)

  private def expandInstr(instr: Instr): List[Instr] = instr match
    case Instr.For(elem @ Expr.Var(_), Expr.Range(low, high), body) =>
      List(
        Instr.Let(elem, low),
        Instr.While(
          Cond.Compare(elem, Cond.CompareOp.Le, high),
          transform(body) :+
          Instr.Set(elem, Expr.BinOp(elem, Expr.BOp.Add, Expr.Num("1"))),
        ),
      )
    case _ =>
      List(instr.mapBody(transform))
