package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Expr, Instr}

/** Rewrites a `"Let |x| be the following steps given argument |V|: ..."` step
  * — parsed by [[ExprParser]] into an `Instr.Let` whose `expr` is a
  * placeholder `Expr.Closure("", params, Nil)` and whose `body` (still on the
  * `Let` itself, exactly like [[ExpandQueueATaskPass]]'s `Perform.body`)
  * holds the substeps — into a `Let` binding a proper closure over a freshly
  * split-off top-level [[Algorithm]]:
  *
  * {{{
  *   Let(Var("onFulfilledSteps"), Closure("", List("V"), Nil), substeps)
  * }}}
  * becomes:
  * {{{
  *   Let(Var("onFulfilledSteps"), Closure(closureName, List("V"), captured), Nil)
  * }}}
  * where `substeps` is split off into a fresh top-level [[Algorithm]] named
  * `closureName` taking `params` (`|V|` here) as its formal parameters, and
  * `captured` is every other free variable `substeps` references — computed
  * by [[FreeVarAnalysis]], excluding `params` themselves, mirroring how
  * [[ExpandQueueATaskPass]] computes captures for a "queue a task" step's
  * substeps.
  *
  * Runs in the same late slot as [[ExpandQueueATaskPass]], for the same
  * reason: `body` rides through every earlier pass as ordinary nested
  * `Let.body` content (every pass already recurses into it via
  * `Instr.mapBody`), so by the time this pass sees it, it's already fully
  * lowered.
  */
object ExpandStepsClosurePass extends LoweringPass:
  private var counter = 0
  private def freshClosureName(): String =
    counter += 1; s"_stepsclosure$counter"

  def run(algos: List[Algorithm]): List[Algorithm] =
    val extra = collection.mutable.ListBuffer.empty[Algorithm]
    val rewritten = algos.map(a => a.copy(body = transform(a.body, extra)))
    rewritten ++ extra.toList

  private def transform(
    instrs: List[Instr],
    extra: collection.mutable.ListBuffer[Algorithm],
  ): List[Instr] = instrs match
    case Nil => Nil
    case Instr.Let(lhs, Expr.Closure(_, params, _), body) :: rest
        if body.nonEmpty =>
      val closureName = freshClosureName()
      val captured =
        (FreeVarAnalysis.freeVars(body) -- params).toList.sorted
      extra += Algorithm(None, Some(closureName), params.map(p => s"|$p|"), "", body)
      Instr.Let(lhs, Expr.Closure(closureName, params, captured), Nil) ::
        transform(rest, extra)
    case instr :: rest =>
      instr.mapBody(transform(_, extra)) :: transform(rest, extra)
