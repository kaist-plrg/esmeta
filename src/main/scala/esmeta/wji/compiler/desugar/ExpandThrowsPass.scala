package esmeta.wji.compiler.desugar

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}
import esmeta.wji.lang.Instr.PerformOutcome

/** Expands a call immediately followed by an `If this throws an exception,
  * catch it, ...` check (`Cond.Throws`, see [[CondParser]]) into an explicit
  * completion-record inspection.
  *
  * Every WJI `Throw` step is planned to compile to a real completion record
  * (mirroring `esmeta.compiler`'s own `ThrowCompletion`/`.Type`/`.Value`
  * convention for ECMA-262), so a call that might throw actually returns that
  * completion record rather than a bare value. This pass makes that explicit
  * at the metalang level:
  *
  * {{{
  *   Perform(f, args, BindResult(x))
  *   IfChain([(Throws(_), catchBody)], fallback = Nil)
  * }}}
  * becomes:
  * {{{
  *   Perform(f, args, BindResult(_throwCompN))
  *   IfChain(
  *     [(Eq(_throwCompN.[[Type]], SpecTerm(throw), false),
  *       Let(|exception|, _throwCompN.[[Value]]) :: catchBody)],
  *     fallback = [Let(x, _throwCompN.[[Value]])],
  *   )
  * }}}
  *
  * The exception `kind` (e.g. `{{TypeError}}` in `Cond.Throws(Some("TypeError"))`)
  * is ignored for now — every `Throws` is treated the same regardless of the
  * specific type named.
  *
  * Must run after [[GroupIfChainPass]] (so the check is already an
  * `IfChain`) and after [[ExtractInlineAlgoCallPass]] (so the preceding call
  * is already a `Perform`). Only fires when that `Perform` has no `body` of
  * its own — true for every occurrence seen in the spec so far; otherwise the
  * pattern is left alone for a future pass.
  */
object ExpandThrowsPass extends DesugarPass:
  private var counter = 0
  private def freshComp(): String = { counter += 1; s"_throwComp$counter" }

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = transform(a.body)))

  private def stripPipes(s: String): String = s.stripPrefix("|").stripSuffix("|")

  private def transform(instrs: List[Instr]): List[Instr] = instrs match
    case Nil => Nil
    case (call: Instr.Perform) ::
        Instr.IfChain(List((Cond.Throws(_), catchBody)), Nil) ::
        rest if call.body.isEmpty =>
      val tmpName = freshComp()
      val tmp = Expr.Var(tmpName)
      val completionCall = call.copy(outcome = PerformOutcome.BindResult(tmpName))
      val isThrow =
        Cond.Eq(Expr.Field(tmp, "Type"), Expr.SpecTerm("throw"), negated = false)
      val catchBranch =
        Instr.Let(Expr.Var("exception"), Expr.Field(tmp, "Value")) ::
        transform(catchBody)
      val fallback: List[Instr] = call.outcome match
        case PerformOutcome.BindResult(v) =>
          List(Instr.Let(Expr.Var(stripPipes(v)), Expr.Field(tmp, "Value")))
        case PerformOutcome.Discard => Nil
        case PerformOutcome.ReturnResult =>
          List(Instr.Return(Some(Expr.Field(tmp, "Value"))))
      completionCall ::
      Instr.IfChain(List((isThrow, catchBranch)), fallback) ::
      transform(rest)
    case instr :: rest =>
      instr.mapBody(transform) :: transform(rest)
