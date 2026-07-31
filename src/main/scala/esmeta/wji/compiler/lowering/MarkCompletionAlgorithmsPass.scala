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
    *
    * Must precede:
    *   - [[ExpandAbruptPass]]: needs `Expr.Abrupt` `?`/`!` markers still
    *     present to detect.
    */
  override def requires: Set[LoweringPass] =
    Set(ResolveLinksPass, GroupIfChainPass)
  override def mustPrecede: Set[LoweringPass] =
    Set(ExpandAbruptPass)

  def run(algos: List[Algorithm]): List[Algorithm] =
    val completionAlgos = CompletionAlgorithms.compute(algos)
    algos.map { a =>
      val name = a.name.orElse(a.id).map(_.toLowerCase)
      if name.exists(completionAlgos.contains) then
        a.copy(returnsCompletion = true)
      else a
    }
