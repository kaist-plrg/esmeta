package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}
import esmeta.wji.lang.Instr.PerformOutcome

/** Expands a call immediately followed by an `If this throws an exception,
  * catch it, ...` check (`Cond.Throws`, see [[CondParser]]) into an explicit
  * completion-record inspection.
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
  *   IfChain([(Throws(_), catchBody)], fallback = Nil)
  * }}}
  * becomes:
  * {{{
  *   Perform(f, args, BindResult(_throwCompN))
  *   IfChain(
  *     [(_throwCompN has field "Type",
  *       [IfChain(
  *           [(Eq(_throwCompN.[[Type]], SpecTerm(throw), false),
  *             Let(|exception|, _throwCompN.[[Value]]) :: catchBody)],
  *           fallback = [Let(x, _throwCompN.[[Value]])],   // completion, not a throw
  *         )])],
  *     fallback = [Let(x, _throwCompN)],                   // not a completion at all
  *   )
  * }}}
  *
  * The exception `kind` (e.g. `{{TypeError}}` in
  * `Cond.Throws(Some("TypeError"))`) is ignored for now — every `Throws` is
  * treated the same regardless of the specific type named.
  *
  * Must run after [[GroupIfChainPass]] (so the check is already an `IfChain`)
  * and after [[ExtractInlineAlgoCallPass]] (so the preceding call is already a
  * `Perform`). Only fires when that `Perform` has no `body` of its own — true
  * for every occurrence seen in the spec so far; otherwise the pattern is left
  * alone for a future pass.
  */
object ExpandThrowsPass extends LoweringPass:
  private var counter = 0
  private def freshComp(): String = { counter += 1; s"_throwComp$counter" }

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = transform(a.body)))

  private def stripPipes(s: String): String =
    s.stripPrefix("|").stripSuffix("|")

  private def transform(instrs: List[Instr]): List[Instr] = instrs match
    case Nil => Nil
    case (call: Instr.Perform) ::
        Instr.IfChain(List((Cond.Throws(_), catchBody)), Nil) ::
        rest if call.body.isEmpty =>
      val tmpName = freshComp()
      val tmp = Expr.Var(tmpName)
      val completionCall =
        call.copy(outcome = PerformOutcome.BindResult(tmpName))

      // the same result binding (or discard/return), just fed a different
      // expression depending on which of the three shapes it turned out to be
      def bind(result: Expr): List[Instr] = call.outcome match
        case PerformOutcome.BindResult(v) =>
          List(Instr.Let(Expr.Var(stripPipes(v)), result))
        case PerformOutcome.Discard      => Nil
        case PerformOutcome.ReturnResult => List(Instr.Return(Some(result)))

      val isThrow =
        Cond.Eq(
          Expr.Field(tmp, "Type"),
          Expr.SpecTerm("throw"),
          negated = false,
        )
      val catchBranch =
        Instr.Let(Expr.Var("exception"), Expr.Field(tmp, "Value")) ::
        transform(catchBody)
      val completionCheck = Instr.IfChain(
        List((isThrow, catchBranch)),
        bind(Expr.Field(tmp, "Value")), // a completion, but not a throw
      )

      completionCall ::
      Instr.IfChain(
        List((Cond.HasField(Expr.Field(tmp, "Type")), List(completionCheck))),
        bind(tmp), // not a completion at all — a bare value
      ) ::
      transform(rest)
    case instr :: rest =>
      instr.mapBody(transform) :: transform(rest)
