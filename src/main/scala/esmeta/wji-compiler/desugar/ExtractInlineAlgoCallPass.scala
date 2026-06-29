package esmeta.wji.compiler.desugar

import esmeta.wji.lang.{Algorithm, Expr, Instr}
import esmeta.wji.lang.Instr.PerformOutcome

/** Converts `AlgoCall`-with-args that appear in expression position into
  * explicit `Perform` statements so the compiler can emit `ICall` instead
  * of `EYet("call ...")`.
  *
  * Handles:
  * {{{
  *   Let(x, AlgoCall(f, args), body)                   → Perform(f, args, BindResult(x), body)
  *   Let(x, Abrupt("!", AlgoCall(f, args)), body)       → same  (strips ! wrapper)
  *   Return(Some(AlgoCall(f, args)), body)              → Perform(f, args, ReturnResult, body)
  *   Return(Some(Abrupt("!", AlgoCall(f, args))), body) → same
  * }}}
  *
  * Zero-argument `AlgoCall`s are left alone (the compiler emits `ERef(Global(name))`
  * for those, which is already correct).
  *
  * Must run after [[ExpandAbruptPass]] and [[ExpandDestructuringLetPass]]
  * (so `Let(_tupleN, AlgoCall(...))` nodes are visible), and before
  * [[ExpandPerformReturnResultPass]] (which expands the `ReturnResult` outcome).
  */
object ExtractInlineAlgoCallPass extends DesugarPass:

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = transform(a.body)))

  private def transform(instrs: List[Instr]): List[Instr] =
    instrs.flatMap(expandInstr)

  private def expandInstr(instr: Instr): List[Instr] = instr match

    case Instr.Let(Expr.Var(x), rhs, body) =>
      // include zero-arg AlgoCalls: "Let |p| be [=a new promise=]" is a call
      extractCall(rhs, zeroArg = true) match
        case Some((link, args)) =>
          List(Instr.Perform(link, args, PerformOutcome.BindResult(x), transform(body)))
        case None =>
          List(instr.mapBody(transform))

    case Instr.Return(Some(rhs), body) =>
      // zero-arg AlgoCalls in Return are spec value refs (e.g. [=error=]), not calls
      extractCall(rhs, zeroArg = false) match
        case Some((link, args)) =>
          List(Instr.Perform(link, args, PerformOutcome.ReturnResult, transform(body)))
        case None =>
          List(instr.mapBody(transform))

    case _ =>
      List(instr.mapBody(transform))

  /** Unwraps an optional `!` (assert-not-abrupt) wrapper and returns
    * `(link, args)` if the expression is an `AlgoCall`.
    *
    * @param zeroArg if true, zero-argument calls are included (for Let RHS);
    *                if false, only calls with arguments (for Return value).
    */
  private def extractCall(expr: Expr, zeroArg: Boolean): Option[(String, List[Expr])] = expr match
    case Expr.AlgoCall(link, args) if zeroArg || args.nonEmpty =>
      Some((link, args))
    case Expr.Abrupt("!", Expr.AlgoCall(link, args)) if zeroArg || args.nonEmpty =>
      Some((link, args))
    case _ =>
      None
