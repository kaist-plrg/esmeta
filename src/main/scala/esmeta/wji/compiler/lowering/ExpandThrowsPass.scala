package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}
import esmeta.wji.lang.Instr.PerformOutcome

/** Expands a call immediately followed by an `If this throws an exception,
  * catch it, ...` check (`Cond.Throws`, see [[CondParser]]) into an explicit
  * completion-record inspection. Fires on either a named-link call
  * (`Instr.Perform`) or a closure-value call (`Instr.PerformClosure`) — the
  * latter needed once `Cond.Throws.bindTo` (below) lets *any*
  * completion-checked call reuse this machinery, not just named-link
  * `Perform`s; in particular, [[ExpandTryPass]] normalizes WebIDL's "Try
  * running the following steps: ... And then, if an exception |E| was thrown:
  * ..." idiom into exactly this `PerformClosure` shape before this pass ever
  * runs, so no idiom-specific knowledge needs adding here.
  *
  * Every WJI `Throw` step is planned to compile to a real completion record
  * (mirroring `esmeta.compiler`'s own `ThrowCompletion`/`.Type`/`.Value`
  * convention for ECMA-262). But — also mirroring `esmeta.compiler`'s
  * `needRetComp`/`isCompletion` handling — not every call is guaranteed to
  * actually return one: a callee whose own steps can never abrupt just returns
  * its bare value. So the check has to be three-way, exactly like
  * `esmeta.compiler.Compiler`'s `and(isCompletion(x), is(tv, expected))`
  * pattern: first check the result even *has* a `.Type` field at all
  * (`Cond.HasField`) before ever reading it, since reading `.Type` off a
  * non-record value (e.g. the bare list `read the imports` returns today)
  * throws `InvalidObjField` outright.
  *
  * {{{
  *   Perform(f, args, BindResult(x))
  *   IfChain([(Throws(_, Some(bindTo)), catchBody)], fallback = Nil)
  * }}}
  * becomes:
  * {{{
  *   Perform(f, args, BindResult(_throwCompN))
  *   IfChain(
  *     [(_throwCompN has field "Type",
  *       [IfChain(
  *           [(Eq(_throwCompN.[[Type]], SpecTerm(throw), false),
  *             Let(bindTo, _throwCompN.[[Value]]) :: catchBody)],
  *           fallback = [Let(x, _throwCompN.[[Value]])],   // completion, not a throw
  *         )])],
  *     fallback = [Let(x, _throwCompN)],                   // not a completion at all
  *   )
  * }}}
  * (`Instr.PerformClosure` in place of `Instr.Perform` follows the identical
  * shape, just swapping which field carries the callee.)
  *
  * The exception `kind` (e.g. `{{TypeError}}` in
  * `Cond.Throws(Some("TypeError"))`) is ignored for now — every `Throws` is
  * treated the same regardless of the specific type named.
  *
  * Only fires when the preceding call has no `body` of its own — true for every
  * occurrence seen in the spec so far; otherwise the pattern is left alone for
  * a future pass.
  *
  * The outer `Cond.HasField` layer is skipped — `completionCall` followed
  * directly by `completionCheck`, no wrapping `IfChain` or `bind(tmp)` fallback
  * at all — when the call is a named-link `Instr.Perform` whose target is
  * itself a WJI algorithm already known (via [[MarkCompletionAlgorithmsPass]]'s
  * fixed point, see [[CompletionAlgorithms]]) to always return a Completion
  * Record: the "not a completion at all" fallback can then never actually
  * trigger, so emitting the check for it at all would just be permanently dead
  * code (confirmed via `esmeta.wji.WjiCoverage` — `personal/coverage-gaps.md`).
  * A `PerformClosure` call has no statically-known target name to look up this
  * way, so it always keeps the full three-way check.
  *
  * Category: Completion-record convention.
  */
object ExpandThrowsPass extends LoweringPass:

  /** Requires:
    *   - [[GroupIfChainPass]]: needs the check already grouped into an
    *     `Instr.IfChain`, not a raw `If` sibling.
    *   - [[ExpandInlineAlgoCallPass]]: needs the preceding call already
    *     converted to `Instr.Perform`.
    *   - [[MarkCompletionAlgorithmsPass]]: needs `returnsCompletion` already
    *     stamped onto every `Algorithm` to know which named-link targets can
    *     skip the outer `Cond.HasField` check (see the class doc above).
    */
  override def requires: Set[LoweringPass] =
    Set(
      GroupIfChainPass,
      ExpandInlineAlgoCallPass,
      MarkCompletionAlgorithmsPass,
    )

  /** Generates this pass's `_throwCompN` names for a single algorithm. Scoped
    * as a value local to each [[run]] iteration rather than a mutable field on
    * this `object` — the latter is JVM-wide singleton state, so concurrent
    * `run` calls (e.g. multiple ScalaTest suites compiling algorithms in
    * parallel, which sbt's default `Test / parallelExecution` allows) would
    * race on incrementing/resetting a shared counter, producing
    * nondeterministic naming depending on thread interleaving.
    */
  private class Counter:
    private var n = 0
    def freshComp(): String = { n += 1; s"_throwComp$n" }

  def run(algos: List[Algorithm]): List[Algorithm] =
    // names (normalized, lowercased) of every WJI algorithm already proven,
    // via MarkCompletionAlgorithmsPass's own fixed point, to always return a
    // Completion Record — see this class's own doc on what that buys a named
    // call site below.
    val alwaysCompletion: Set[String] =
      algos
        .filter(_.returnsCompletion)
        .flatMap(a => a.name.orElse(a.id))
        .map(_.toLowerCase)
        .toSet
    algos.map { a =>
      val counter = Counter()
      a.copy(body = transform(a.body, counter, alwaysCompletion))
    }

  private def stripPipes(s: String): String =
    s.stripPrefix("|").stripSuffix("|")

  /** `reconstruct` rebuilds the matched call with only its `outcome` changed
    * (`call.copy(outcome = _)`) — shared by the `Instr.Perform`/
    * `Instr.PerformClosure` arms below, which are otherwise identical.
    * `calleeName` is `Some(call.func)` for a named-link `Instr.Perform`
    * (bracketed link-text form — see `CompletionAlgorithms.normalize`'s own
    * doc), `None` for a `PerformClosure` (no statically-known target).
    */
  private def expand(
    reconstruct: PerformOutcome => Instr,
    calleeName: Option[String],
    outcome: PerformOutcome,
    bindTo: Option[String],
    catchBody: List[Instr],
    rest: List[Instr],
    counter: Counter,
    alwaysCompletion: Set[String],
  ): List[Instr] =
    val tmpName = counter.freshComp()
    val tmp = Expr.Var(tmpName)
    val completionCall = reconstruct(PerformOutcome.BindResult(tmpName))

    // the same result binding (or discard/return), just fed a different
    // expression depending on which of the three shapes it turned out to be
    def bind(result: Expr): List[Instr] = outcome match
      case PerformOutcome.BindResult(v) =>
        List(Instr.Let(Expr.Var(stripPipes(v)), result))
      case PerformOutcome.Discard      => Nil
      case PerformOutcome.ReturnResult => List(Instr.Return(Some(result)))

    val isThrow =
      Cond.Eq(Expr.Field(tmp, "Type"), Expr.SpecTerm("throw"), negated = false)
    val catchBranch =
      bindTo match
        case Some(name) =>
          Instr.Let(Expr.Var(stripPipes(name)), Expr.Field(tmp, "Value")) ::
          transform(catchBody, counter, alwaysCompletion)
        case None =>
          transform(catchBody, counter, alwaysCompletion)
    val completionCheck = Instr.IfChain(
      List((isThrow, catchBranch)),
      bind(Expr.Field(tmp, "Value")), // a completion, but not a throw
    )

    val knownAlwaysCompletion =
      calleeName.exists(n =>
        alwaysCompletion.contains(CompletionAlgorithms.normalize(n)),
      )

    val afterCall =
      if knownAlwaysCompletion then
        // the "not a completion at all" fallback below can never trigger —
        // skip the outer HasField wrapper and its dead bind(tmp) branch
        // entirely rather than emitting then relying on a later pass to
        // eliminate it.
        List(completionCheck)
      else
        List(
          Instr.IfChain(
            List(
              (Cond.HasField(Expr.Field(tmp, "Type")), List(completionCheck)),
            ),
            bind(tmp), // not a completion at all — a bare value
          ),
        )

    completionCall :: afterCall ++ transform(rest, counter, alwaysCompletion)

  private def transform(
    instrs: List[Instr],
    counter: Counter,
    alwaysCompletion: Set[String],
  ): List[Instr] =
    instrs match
      case Nil => Nil
      case (call: Instr.Perform) ::
          Instr.IfChain(List((Cond.Throws(_, bindTo), catchBody)), Nil) ::
          rest if call.body.isEmpty =>
        expand(
          o => call.copy(outcome = o),
          Some(call.func),
          call.outcome,
          bindTo,
          catchBody,
          rest,
          counter,
          alwaysCompletion,
        )
      case (call: Instr.PerformClosure) ::
          Instr.IfChain(List((Cond.Throws(_, bindTo), catchBody)), Nil) ::
          rest if call.body.isEmpty =>
        expand(
          o => call.copy(outcome = o),
          None,
          call.outcome,
          bindTo,
          catchBody,
          rest,
          counter,
          alwaysCompletion,
        )
      case instr :: rest =>
        instr.mapBody(transform(_, counter, alwaysCompletion)) ::
        transform(rest, counter, alwaysCompletion)
