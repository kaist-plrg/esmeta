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
  *   Perform(func, args, BindResult("_resultN"), [])
  *   Return(Var("_resultN"))
  * }}}
  *
  * This removes the `ReturnResult` outcome from `Instr.Perform` so the compiler
  * only needs to handle `Discard`/`BindResult` there (unlike
  * `Instr.PerformClosure`, whose `ReturnResult` case `Compiler` compiles
  * directly — see its doc — so this pass deliberately doesn't touch it).
  *
  * Category: Structural desugaring.
  */
object ExpandPerformReturnResultPass extends LoweringPass:

  /** Requires:
    *   - [[ExtractInlineAlgoCallPass]]: needs a `Return(Some(AlgoCall(...)))`
    *     already converted to `Instr.Perform(..., ReturnResult, ...)` — one
    *     that arrives late (after this pass has already run) would never get
    *     simplified, and would reach `Compiler`'s `impossible()` guard for
    *     `Instr.Perform`'s `ReturnResult` case instead.
    */
  override def requires: Set[LoweringPass] = Set(ExtractInlineAlgoCallPass)

  private var counter = 0
  private def freshRet(): String = { counter += 1; s"_result$counter" }

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map { a =>
      counter = 0
      a.copy(body = transform(a.body))
    }

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
