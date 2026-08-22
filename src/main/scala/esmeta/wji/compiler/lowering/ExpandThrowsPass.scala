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
  * Category: Completion-record convention.
  */
object ExpandThrowsPass extends LoweringPass:

  /** Requires:
    *   - [[GroupIfChainPass]]: needs the check already grouped into an
    *     `Instr.IfChain`, not a raw `If` sibling.
    *   - [[ExpandInlineAlgoCallPass]]: needs the preceding call already
    *     converted to `Instr.Perform`.
    */
  override def requires: Set[LoweringPass] =
    Set(GroupIfChainPass, ExpandInlineAlgoCallPass)

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
    algos.map { a =>
      val counter = Counter()
      a.copy(body = transform(a.body, counter))
    }

  private def stripPipes(s: String): String =
    s.stripPrefix("|").stripSuffix("|")

  /** `reconstruct` rebuilds the matched call with only its `outcome` changed
    * (`call.copy(outcome = _)`) — shared by the `Instr.Perform`/
    * `Instr.PerformClosure` arms below, which are otherwise identical.
    */
  private def expand(
    reconstruct: PerformOutcome => Instr,
    outcome: PerformOutcome,
    bindTo: Option[String],
    catchBody: List[Instr],
    rest: List[Instr],
    counter: Counter,
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
          transform(catchBody, counter)
        case None =>
          transform(catchBody, counter)
    val completionCheck = Instr.IfChain(
      List((isThrow, catchBranch)),
      bind(Expr.Field(tmp, "Value")), // a completion, but not a throw
    )

    completionCall ::
    Instr.IfChain(
      List((Cond.HasField(Expr.Field(tmp, "Type")), List(completionCheck))),
      bind(tmp), // not a completion at all — a bare value
    ) ::
    transform(rest, counter)

  private def transform(instrs: List[Instr], counter: Counter): List[Instr] =
    instrs match
      case Nil => Nil
      case (call: Instr.Perform) ::
          Instr.IfChain(List((Cond.Throws(_, bindTo), catchBody)), Nil) ::
          rest if call.body.isEmpty =>
        expand(
          o => call.copy(outcome = o),
          call.outcome,
          bindTo,
          catchBody,
          rest,
          counter,
        )
      case (call: Instr.PerformClosure) ::
          Instr.IfChain(List((Cond.Throws(_, bindTo), catchBody)), Nil) ::
          rest if call.body.isEmpty =>
        expand(
          o => call.copy(outcome = o),
          call.outcome,
          bindTo,
          catchBody,
          rest,
          counter,
        )
      case instr :: rest =>
        instr.mapBody(transform(_, counter)) :: transform(rest, counter)
