package esmeta.wji.compiler.lowering

import esmeta.wji.lang.Algorithm
import esmeta.error.{PipelineOrderError, UnsupportedSpecShape}

/** Runs the WJI lowering pipeline: a fixed sequence of [[LoweringPass]]es that
  * rewrite spec-text-shaped `Algorithm`s into a form
  * [[esmeta.wji.compiler.Compiler]] can compile directly.
  *
  * Every pass falls into one of seven categories (see each pass's own doc
  * comment for its specific tag). The middle three are all sub-kinds of what
  * used to be one "Structural desugaring" bucket — split because they behave
  * differently enough to be worth telling apart: '''Elimination''' is the only
  * one of the three that provably removes a construct (see each such pass's own
  * postconditions/[[AstQuery]] use, where present). The last two are sub-kinds
  * of what used to be one "Spec-dependent" bucket — split because the knowledge
  * each relies on, while equally absent from spec prose itself, comes from a
  * different place: SpecTec's own OCaml runtime source vs. WJI's own choices
  * for how to represent something at runtime.
  *   - '''Housekeeping''': identity-level cleanup (naming, dead-link
  *     resolution, note-stripping) with no semantic effect on control flow.
  *   - '''Structural desugaring — Reordering''': reshapes the *arrangement* of
  *     already-valid nodes (ANF-style evaluation-order hoisting, flat
  *     `If`/`ElseIf`/`Else` siblings into a tree) without desugaring any
  *     construct away.
  *   - '''Structural desugaring — Elimination''': recognizes one explicit spec
  *     construct (`for each`, destructuring `Let`, an inline algorithm call,
  *     ...) and rewrites it into lower-level syntax, so that construct no
  *     longer appears afterward. The largest group, and the bulk of the
  *     pipeline overall.
  *   - '''Structural desugaring — Injection''': adds calling-convention
  *     machinery (builtin/interface-member argument unpacking, completion
  *     wrapping) that has no corresponding spec-text construct to eliminate — a
  *     WJI/ESMeta implementation requirement, not a spec idiom being desugared.
  *   - '''Completion-record convention''': the one piece of *implicit*
  *     spec-wide behavior — ECMA-262/Infra's automatic abrupt-completion
  *     propagation — made explicit as real control flow.
  *   - '''Spec-dependent — SpecTec''': correctness relies on knowing a specific
  *     `CaseV` tag string, embedding/numerics function name, or nesting shape
  *     SpecTec's own runtime (`spectec/spectec/src/
  *     backend-interpreter/construct.ml`'s `al_of_*`/`al_to_*`,
  *     `backend-server/server.ml`'s dispatch) actually uses — knowledge spec
  *     prose doesn't spell out directly (e.g. a numeric const's nested numtype
  *     tag, or that `signed_N`'s inverse is called `inv_signed_N`). None of
  *     this would look the same if WJI mechanized a different spec.
  *   - '''Spec-dependent — WJI''': correctness relies on knowing how WJI itself
  *     chose to represent something at runtime, rather than on anything
  *     SpecTec's own runtime does — e.g. recognizing that "is an Exported
  *     Function"/"is an Exported GC Object" collapse to a `Cond.HasSlot` check
  *     specifically because WJI represents these WebIDL exotic-object kinds as
  *     ordinary records carrying the internal slot their own spec definition
  *     already names, or that "is a regular/static operation" collapses to a
  *     `Cond.Eq` against an `operation` record's own `kind` field because WJI
  *     seeds that field directly from `esmeta.wji.lang.MemberKind` — not
  *     because of anything about SpecTec.
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
    // prepare
    ResolveTypeAnnotationPass,
    ElideHtmlHostHooksPass,
    DropNotesPass,
    ResolveLinksPass,
    GroupIfChainPass,
    NormalizeSpecTecCaseShapePass,
    ExpandFollowingStepsPass,
    // injections
    MarkBuiltinBehaviourPass,
    AddBuiltinBehaviourPass,
    AddInterfaceMemberBuiltinBehaviourPass,
    // eliminations
    ExpandTryPass,
    ExpandHasDuplicatesPass,
    ExpandRemovePass,
    ExpandConditionalPass,
    ExpandGetMemberPass,
    ExpandForEachPass,
    ExpandForPass,
    ExpandNewByteSequencePass,
    ExpandIndexOfPass,
    ExpandShortestArgumentListPass,
    ExpandAllocationFailsPass,
    ExpandDestructuringLetPass,
    ExpandExistentialsPass,
    ExpandDataBlockOfPass,
    ExpandNewArrayBufferPass,
    ExpandIsOfFormPass,
    ExpandAbbreviatedCondPass,
    ExpandWjiIsTypePass,
    ExpandExposedPass,
    MarkCompletionAlgorithmsPass,
    NormalizeEvaluationOrderPass, // normalization point
    PropagateUnguardedCallsPass,
    ExpandAbruptPass,
    ExpandInlineAlgoCallPass,
    ExpandClosureCallPass,
    ExpandThrowsPass,
    ExpandPerformReturnResultPass,
    InsertFallthroughReturnPass,
    WrapCompletionReturnsPass,
    ExpandMatchesExistsPass,
    ExpandQueueATaskPass,
    // cleanup
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

  /** Throws [[esmeta.error.UnsupportedSpecShape]] naming `pass` and `phase`
    * (`"precondition"`/`"postcondition"`) for the first [[Condition]] in
    * `conditions` whose `holds` returns `false` against `algos` — see
    * [[LoweringPass.preconditions]]/[[LoweringPass.postconditions]]'s own doc
    * for why this is a distinct check from [[validate]]'s ordering one.
    */
  private def checkConditions(
    pass: LoweringPass,
    phase: String,
    conditions: List[Condition],
    algos: List[Algorithm],
  ): Unit =
    conditions.find(c => !c.holds(algos)).foreach { c =>
      throw UnsupportedSpecShape(
        pass.name,
        s"$phase violated: ${c.description}",
      )
    }

  def run(algos: List[Algorithm]): List[Algorithm] =
    validate()
    pipeline.foldLeft(algos) { (acc, pass) =>
      checkConditions(pass, "precondition", pass.preconditions, acc)
      val result = pass.run(acc)
      checkConditions(pass, "postcondition", pass.postconditions, result)
      result
    }
