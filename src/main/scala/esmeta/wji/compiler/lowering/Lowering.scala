package esmeta.wji.compiler.lowering

import esmeta.wji.lang.Algorithm
import esmeta.error.PipelineOrderError

/** Runs the WJI lowering pipeline: a fixed sequence of [[LoweringPass]]es that
  * rewrite spec-text-shaped `Algorithm`s into a form
  * [[esmeta.wji.compiler.Compiler]] can compile directly.
  *
  * Every pass falls into one of three categories (see each pass's own doc
  * comment for its specific tag):
  *   - '''Housekeeping''': identity-level cleanup (naming, dead-link
  *     resolution, note-stripping) with no semantic effect on control flow.
  *   - '''Structural desugaring''': re-expresses an explicit spec construct
  *     (`for each`, destructuring `Let`, inline algorithm calls, ...) into
  *     lower-level syntax. This is the bulk of the pipeline.
  *   - '''Completion-record convention''': the one piece of *implicit*
  *     spec-wide behavior — ECMA-262/Infra's automatic abrupt-completion
  *     propagation — made explicit as real control flow.
  *
  * The category grouping is documentation only, not machine-checked: unlike
  * ordering (see [[LoweringPass.requires]]/[[LoweringPass.mustPrecede]] and
  * [[validate]]), a pass being miscategorized can't silently produce a wrong
  * compile, so there's no correctness reason to enforce it in code.
  * [[pipeline]] below is *not* grouped by category — several passes need to
  * interleave (e.g. a Structural pass needs something a Completion pass
  * produces, followed by another Structural pass that needs the result) — so
  * don't read adjacency in [[pipeline]] as a category boundary.
  */
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
    ExpandIndexOfPass,
    ExpandDestructuringLetPass,
    NormalizeAlgoCallPass,
    ExtractInlineAlgoCallPass,
    ExpandClosureCallPass,
    ExpandThrowsPass,
    ExpandIsOfFormPass,
    ExpandPerformReturnResultPass,
    InsertFallthroughReturnPass,
    WrapCompletionReturnsPass,
    PropagateUnguardedCallsPass,
    ExpandAbbreviatedCondPass,
    ExpandMatchesExistsPass,
    ExpandFollowingStepsPass,
    AddBuiltinBehaviourPass,
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
