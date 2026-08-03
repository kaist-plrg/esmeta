package esmeta.wji.compiler.lowering

import esmeta.wji.lang.Algorithm

/** Runs [[CompletionAlgorithms.compute]] once and stamps its result onto each
  * [[Algorithm]]'s own `returnsCompletion` field, so every later pass
  * (`WrapCompletionReturnsPass`, `PropagateUnguardedCallsPass`) can just read
  * it off the `Algorithm`s it's already handed, rather than needing the
  * computed set threaded through as a constructor parameter — keeps this a
  * perfectly ordinary `List[LoweringPass]` entry like every other pass.
  *
  * See `CompletionAlgorithms`'s own doc for why the analysis needs to observe
  * that particular shape.
  *
  * Category: Completion-record convention.
  */
object MarkCompletionAlgorithmsPass extends LoweringPass:

  /** Requires:
    *   - [[ResolveLinksPass]]: needs call targets already resolved to real
    *     names (`Expr.AlgoCall`, not a raw `Expr.Link`) for
    *     `CompletionAlgorithms`'s call-graph analysis to recognize them.
    *   - [[GroupIfChainPass]]: needs a `Cond.Throws` catch already grouped into
    *     an `Instr.IfChain` — the one shape `CompletionAlgorithms` itself
    *     recognizes for that idiom (see its own class doc).
    *   - [[ExpandFollowingStepsPass]]: needs a "following steps" closure
    *     already split off into its own top-level `Algorithm` so it gets
    *     analyzed (and `returnsCompletion`-stamped) on its own, independently
    *     of the algorithm it was textually nested in — otherwise the closure's
    *     name doesn't exist in `algos` yet at all, and it silently never gets
    *     stamped (defaulting to `returnsCompletion = false`, which happens to
    *     be right for e.g. `create a host function`'s own hostfunc closure, but
    *     only by accident of never being analyzed).
    *
    * Must precede:
    *   - [[ExpandAbruptPass]]: needs `Expr.Abrupt` `?`/`!` markers still
    *     present to detect.
    *   - [[NormalizeEvaluationOrderPass]]: needs a marked call still in its
    *     original `Expr.Abrupt(marker, Expr.AlgoCall(...))` shape —
    *     `NormalizeEvaluationOrderPass` hoists the `AlgoCall` out into a
    *     preceding `Let`, leaving `Expr.Abrupt(marker, Var(...))` behind, a
    *     shape `CompletionAlgorithms.callInfo` doesn't recognize as a marked
    *     call at all. Once that split happens, a legitimately `?`/`!`-marked
    *     call looks identical to a bare unmarked one, so `hasUnguardedCallInto`
    *     wrongly treats it as an unhandled call into a completion algorithm
    *     (see e.g. `create a host function`'s hostfunc closure, whose
    *     `!`-marked `ToWebAssemblyValue` call was wrongly flagged this way once
    *     this pass ran after the split).
    */
  override def requires: Set[LoweringPass] =
    Set(ResolveLinksPass, GroupIfChainPass, ExpandFollowingStepsPass)
  override def mustPrecede: Set[LoweringPass] =
    Set(ExpandAbruptPass, NormalizeEvaluationOrderPass)

  def run(algos: List[Algorithm]): List[Algorithm] =
    val completionAlgos = CompletionAlgorithms.compute(algos)
    algos.map { a =>
      val name = a.name.orElse(a.id).map(_.toLowerCase)
      if name.exists(completionAlgos.contains) then
        a.copy(returnsCompletion = true)
      else a
    }
