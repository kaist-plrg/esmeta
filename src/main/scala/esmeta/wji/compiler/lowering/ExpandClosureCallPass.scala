package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Expr, Instr}
import esmeta.wji.lang.Instr.PerformOutcome

/** Converts a `ClosureCall` that appears in `Let`/`Return` RHS position into an
  * explicit `PerformClosure` statement, so the compiler can emit a real `ICall`
  * instead of `EYet("call ...")` — mirrors [[ExtractInlineAlgoCallPass]] for
  * `AlgoCall`/`JSCall`, except a `ClosureCall`'s callee is a dynamic `Expr`
  * (the closure *value*, e.g. a `|var|` parameter such as
  * `|onFullfilledStepsArg|`), not a static `[=link=]` name, so it can't reuse
  * `Instr.Perform` (whose `func: String` field only fits a name) and needs its
  * own `Instr.PerformClosure` case instead.
  *
  * Handles:
  * {{{
  *   Let(Var(x), ClosureCall(closure, args), body)   → PerformClosure(closure, args, BindResult(x), body)
  *   Return(Some(ClosureCall(closure, args)), body)  → PerformClosure(closure, args, ReturnResult, body)
  * }}}
  *
  * Must run after [[ExpandAbruptPass]] (so a `Abrupt(marker, ClosureCall(...))`
  * direct-RHS wrapper has already been unwrapped into a plain `Let` binding —
  * `ExpandAbruptPass.expand` binds its unwrapped `inner` via a fresh `Let`
  * regardless of what `inner` is, so no separate `Abrupt`-stripping is needed
  * here).
  *
  * Only handles the direct top-level RHS position — like
  * `NormalizeAlgoCallPass` does for `AlgoCall`/`JSCall`, a `ClosureCall` nested
  * inside a larger expression (e.g. as a `BinOp` operand) would need ANF-style
  * hoisting first; no spec text seen so far nests one that way, so this is left
  * as a known gap rather than speculatively generalized.
  */
object ExpandClosureCallPass extends LoweringPass:

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = transform(a.body)))

  private def transform(instrs: List[Instr]): List[Instr] =
    instrs.flatMap(expandInstr)

  private def expandInstr(instr: Instr): List[Instr] = instr match

    case Instr.Let(Expr.Var(x), Expr.ClosureCall(closure, args), body) =>
      List(
        Instr.PerformClosure(
          closure,
          args,
          PerformOutcome.BindResult(x),
          transform(body),
        ),
      )

    case Instr.Return(Some(Expr.ClosureCall(closure, args)), body) =>
      List(
        Instr.PerformClosure(
          closure,
          args,
          PerformOutcome.ReturnResult,
          transform(body),
        ),
      )

    case _ =>
      List(instr.mapBody(transform))
