package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Expr, Instr}
import esmeta.wji.lang.Instr.PerformOutcome

/** Expands `Perform(..., ReturnResult, body)` — the "perform X and return the
  * result" pattern — into an explicit bind + return.
  *
  * {{{
  *   Perform(func, args, ReturnResult, body)
  * }}}
  * becomes:
  * {{{
  *   <body>
  *   Perform(func, args, BindResult("_retN"), [])
  *   Return(Var("_retN"))
  * }}}
  *
  * This removes the `ReturnResult` outcome from the AST so the compiler only
  * needs to handle `Discard` and `BindResult`.
  */
object ExpandPerformReturnResultPass extends LoweringPass:
  private var counter = 0
  private def freshRet(): String = { counter += 1; s"_ret$counter" }

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = transform(a.body)))

  private def transform(instrs: List[Instr]): List[Instr] =
    instrs.flatMap(expandInstr)

  private def expandInstr(instr: Instr): List[Instr] = instr match
    case Instr.Perform(func, args, PerformOutcome.ReturnResult, body) =>
      val tmp = freshRet()
      transform(body) ++
      List(
        Instr.Perform(func, args, PerformOutcome.BindResult(tmp)),
        Instr.Return(Some(Expr.Var(tmp))),
      )
    case _ =>
      List(instr.mapBody(transform))
