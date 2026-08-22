package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}
import esmeta.wji.lang.Instr.PerformOutcome

/** Normalizes WebIDL's "Try running the following steps: ... And then, if an
  * exception |E| was thrown: ..." idiom (`Instr.Try`/`Instr.Catch`, produced by
  * `InstrParser`/`AlgorithmExtractor.parseBody`) into the exact same shape
  * [[ExpandThrowsPass]] already expands for WebIDL's other, already-supported
  * "If this operation throws an exception, catch it, ..." idiom — a call
  * immediately followed by a `Cond.Throws`-guarded `IfChain` — so that pass's
  * completion-record expansion (three-way not-a-completion /
  * completion-but-not-throw / throw check) is reused as-is, with no
  * idiom-specific knowledge added to it.
  *
  * {{{
  *   Try(ClosureCall(closure, args), Nil)
  *   Catch(exceptionVar, catchBody)
  * }}}
  * becomes:
  * {{{
  *   PerformClosure(closure, args, ReturnResult)
  *   IfChain([(Cond.Throws(None, Some(exceptionVar)), catchBody)], Nil)
  * }}}
  *
  * `PerformOutcome.ReturnResult` is what makes a successful (non-throw)
  * completion's value become the *enclosing* algorithm's own return once
  * `ExpandThrowsPass` runs — the same way `return` inside a real `try` exits
  * the enclosing procedure, not just the try. This matches the only two real
  * occurrences, where `Try`/`Catch` is the entire content of its own closure
  * and nothing follows it there — guarded explicitly (`rest.isEmpty`), so a
  * future spec addition that puts more steps after a `Try`/`Catch` pair in the
  * same body falls through unconsumed instead of silently mis-lowering
  * (surfacing loudly later via `Compiler`'s `impossible()` guard on a bare
  * `Instr.Try`/`Instr.Catch`).
  *
  * Runs before [[NormalizeEvaluationOrderPass]] (the pipeline's ANF
  * normalization point) rather than alongside [[ExpandThrowsPass]] itself, even
  * though the two are otherwise adjacent in purpose: `Instr.Try.call` holds a
  * raw `Expr.ClosureCall` sitting in a bespoke instruction field, not yet
  * hoisted into a statement-level call the way every other call is by that
  * point — exactly the shape `NormalizeEvaluationOrderPass` exists to eliminate
  * everywhere else in the program. Running this pass first means
  * `Instr.Try`/`Instr.Catch` are already gone by the normalization point, so
  * nothing downstream (including `NormalizeEvaluationOrderPass` itself) ever
  * needs to special-case them.
  *
  * Category: Structural desugaring — Elimination.
  */
object ExpandTryPass extends LoweringPass:

  /** Requires:
    *   - [[ExpandFollowingStepsPass]]: needs a `Try`'s `call` already hoisted
    *     into `ClosureCall(Closure(...), Nil)`, not `ClosureCall(
    *     FollowingSteps(...), Nil)`.
    */
  override def requires: Set[LoweringPass] = Set(ExpandFollowingStepsPass)

  /** Must precede:
    *   - [[NormalizeEvaluationOrderPass]]: see class doc — `Instr.Try.call`
    *     must not still hold a raw, un-hoisted `Expr.ClosureCall` once ANF
    *     normalization is supposed to hold everywhere.
    *   - [[ExpandThrowsPass]]: is what actually expands the `PerformClosure ::
    *     IfChain(Cond.Throws)` shape this pass produces.
    */
  override def mustPrecede: Set[LoweringPass] =
    Set(NormalizeEvaluationOrderPass, ExpandThrowsPass)

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = transform(a.body)))

  private def transform(instrs: List[Instr]): List[Instr] =
    instrs match
      case Nil => Nil
      case Instr.Try(Expr.ClosureCall(closure, args), Nil) ::
          Instr.Catch(exceptionVar, catchBody) ::
          rest if rest.isEmpty =>
        Instr.PerformClosure(closure, args, PerformOutcome.ReturnResult) ::
        Instr.IfChain(
          List((Cond.Throws(None, Some(exceptionVar)), transform(catchBody))),
          Nil,
        ) :: transform(rest)
      case instr :: rest =>
        instr.mapBody(transform) :: transform(rest)
