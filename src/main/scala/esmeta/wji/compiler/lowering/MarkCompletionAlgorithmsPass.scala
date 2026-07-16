package esmeta.wji.compiler.lowering

import esmeta.wji.lang.Algorithm

/** Runs [[CompletionAlgorithms.compute]] once and stamps its result onto each
  * [[Algorithm]]'s own `returnsCompletion` field, so every later pass
  * (`WrapCompletionReturnsPass`, `PropagateUnguardedCallsPass`) can just read
  * it off the `Algorithm`s it's already handed, rather than needing the
  * computed set threaded through as a constructor parameter — keeps this a
  * perfectly ordinary `List[LoweringPass]` entry like every other pass.
  *
  * Must run right after `ResolveLinksPass` (so call targets are already
  * resolved names) and before `GroupIfChainPass`/`ExpandAbruptPass` — see
  * `CompletionAlgorithms`'s own doc for why the analysis needs to observe that
  * particular shape.
  */
object MarkCompletionAlgorithmsPass extends LoweringPass:
  def run(algos: List[Algorithm]): List[Algorithm] =
    val completionAlgos = CompletionAlgorithms.compute(algos)
    algos.map { a =>
      val name = a.name.orElse(a.id).map(_.toLowerCase)
      if name.exists(completionAlgos.contains) then
        a.copy(returnsCompletion = true)
      else a
    }
