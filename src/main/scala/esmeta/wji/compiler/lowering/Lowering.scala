package esmeta.wji.compiler.lowering

import esmeta.wji.lang.Algorithm
import esmeta.error.PipelineOrderError

object Lowering:
  val pipeline: List[LoweringPass] = List(
    ElideHtmlHostHooksPass,
    ResolveLinksPass,
    MarkCompletionAlgorithmsPass,
    DropNotesPass,
    GroupIfChainPass,
    ExpandHasDuplicatesPass,
    ExpandAbruptPass,
    ExpandForEachPass,
    ExpandForPass,
    ExpandNewByteSequencePass,
    ExpandDestructuringLetPass,
    InsertFallthroughReturnPass,
    WrapCompletionReturnsPass,
    NormalizeAlgoCallPass,
    ExtractInlineAlgoCallPass,
    ExpandClosureCallPass,
    ExpandThrowsPass,
    PropagateUnguardedCallsPass,
    ExpandIsOfFormPass,
    ExpandPerformReturnResultPass,
    ExpandAbbreviatedCondPass,
    ExpandMatchesExistsPass,
    ExpandFollowingStepsPass,
    ExpandQueueATaskPass,
    NormalizeAlgoNamePass,
  )

  /** Checks every pass's declared `requires`/`mustPrecede` (see
    * [[LoweringPass]]) against `pipeline`'s actual order, throwing
    * [[esmeta.error.PipelineOrderError]] on the first violation found — either
    * a referenced pass missing from `pipeline` entirely, or one present but on
    * the wrong side. Run once, from [[run]], before any algorithm is touched.
    */
  private def validate(): Unit =
    val indexOf: Map[LoweringPass, Int] = pipeline.zipWithIndex.toMap

    def indexOrThrow(referrer: LoweringPass, relation: String)(
      target: LoweringPass,
    ): Int =
      indexOf.getOrElse(
        target,
        throw PipelineOrderError(
          s"${referrer.name} $relation ${target.name}, but ${target.name} isn't in Lowering.pipeline at all",
        ),
      )

    for (pass, i) <- pipeline.zipWithIndex do
      for dep <- pass.requires do
        val j = indexOrThrow(pass, "requires")(dep)
        if j >= i then
          throw PipelineOrderError(
            s"${pass.name} (position ${i + 1}) requires ${dep.name} " +
            s"(position ${j + 1}) to run earlier in Lowering.pipeline, but it doesn't",
          )
      for successor <- pass.mustPrecede do
        val j = indexOrThrow(pass, "must run before")(successor)
        if j <= i then
          throw PipelineOrderError(
            s"${pass.name} (position ${i + 1}) must run before ${successor.name} " +
            s"(position ${j + 1}) in Lowering.pipeline, but it doesn't",
          )

  def run(algos: List[Algorithm]): List[Algorithm] =
    validate()
    pipeline.foldLeft(algos)((acc, pass) => pass.run(acc))
