package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Cond, Expr, Instr}
import esmeta.wji.lang.Instr.PerformOutcome

/** Shared logic for wrapping every `Return`/`Throw` exit path of an
  * algorithm's body in an ECMA-262 Completion Record: converts a
  * `Instr.Throw` into a real `ThrowCompletion` return, and wraps every other
  * `Instr.Return` in `NormalCompletion` unless the value being returned is
  * already itself a completion (checked at runtime via `HasField`, mirroring
  * mainline `esmeta.compiler.Compiler`'s own `isCompletion(x)` guard on
  * `ReturnStep`) — so a caller can always assume the result has a `.Type`
  * field.
  *
  * {{{
  *   If(cond, [Throw("a {{TypeError}} exception")])
  *   Return(Some(expr))
  * }}}
  * becomes:
  * {{{
  *   If(cond, [
  *     Perform("__NEW_ERROR_OBJ__", [Str("%TypeError.prototype%")], BindResult(_err1)),
  *     Perform("ThrowCompletion", [Var(_err1)], BindResult(_ret1)),
  *     Return(Some(Var(_ret1))),
  *   ])
  *   Let(_v1, expr)
  *   If(_v1 has field "Type", [Return(Some(_v1))])
  *   Else [
  *     Perform("NormalCompletion", [_v1], BindResult(_ret2)),
  *     Return(Some(Var(_ret2))),
  *   ]
  * }}}
  *
  * Only recognizes a `Throw` target of the exact `"a {{Iface}}"` / `"a
  * {{Iface}} exception"` shape (every occurrence reached so far); any other
  * phrasing is left as `Instr.Unknown` rather than guessed at. Always emits
  * already-compiler-ready `Instr.Perform(..., BindResult(_))` + `Instr.Return`
  * — never an inline `Expr.AlgoCall` — so a caller never needs a later pass
  * (`ExtractInlineAlgoCallPass`/`ExpandPerformReturnResultPass`) to finish the
  * job for it.
  *
  * Kept as a plain object rather than folded into [[WrapCompletionReturnsPass]]
  * itself — mirrors [[CompletionAlgorithms]]'s own split from
  * [[MarkCompletionAlgorithmsPass]] — because it has two independent callers
  * that decide *which* algorithms need it in very different ways:
  *   - [[WrapCompletionReturnsPass]] targets every `Algorithm` with
  *     `returnsCompletion = true`, a whole-program fixed point over "can this
  *     algorithm itself abruptly complete" (see [[CompletionAlgorithms]]).
  *   - [[AddBuiltinBehaviourPass]] targets a `CreateBuiltinFunction`-behaviour
  *     closure unconditionally, regardless of that fixed point (mainline's
  *     own `esmeta.compiler.Compiler.compile`'s `Builtin => needRetComp =
  *     true` — a calling-convention requirement, not a "can it throw" one),
  *     for an `Algorithm` that didn't even exist yet when
  *     `WrapCompletionReturnsPass`'s own `algos.map` ran.
  */
object CompletionWrapping:

  private var counter = 0
  private def freshErr(): String = { counter += 1; s"_err$counter" }
  private def freshVal(): String = { counter += 1; s"_v$counter" }
  private def freshRet(): String = { counter += 1; s"_ret$counter" }

  // "a {{TypeError}} exception" / "a {{TypeError}} exception." / "a {{TypeError}}"
  private val ThrowTarget =
    """(?si)^an?\s+\{\{([^}]+)\}\}(?:\s+exception)?\.?$""".r

  def expand(instrs: List[Instr]): List[Instr] =
    instrs.flatMap(expandInstr)

  private def expandInstr(instr: Instr): List[Instr] = instr match
    // a "the following steps..." closure's substeps are a separate
    // algorithm-to-be (see CompletionAlgorithms) — its own Returns are none
    // of this algorithm's business to wrap. (A no-op by the time
    // AddBuiltinBehaviourPass calls this: no FollowingSteps remain anywhere
    // in the program past ExpandFollowingStepsPass.)
    case i @ Instr.Let(_, Expr.FollowingSteps(_), _) => List(i)
    case Instr.Throw(ThrowTarget(iface), _) =>
      val err = freshErr()
      val ret = freshRet()
      List(
        Instr.Perform(
          "__NEW_ERROR_OBJ__",
          List(Expr.Str(s"%$iface.prototype%")),
          PerformOutcome.BindResult(err),
        ),
        Instr.Perform(
          "ThrowCompletion",
          List(Expr.Var(err)),
          PerformOutcome.BindResult(ret),
        ),
        Instr.Return(Some(Expr.Var(ret))),
      )
    case t: Instr.Throw =>
      List(Instr.Unknown(s"throw ${t.target}"))
    case Instr.Return(Some(expr), _) =>
      // a bare Var (the common case now that ExtractInlineAlgoCallPass/
      // ExpandPerformReturnResultPass already ran) needs no re-binding —
      // testing/returning it directly (instead of aliasing it under a fresh
      // name first) keeps its name intact for PropagateUnguardedCallsPass's
      // own `isAbsorbed` name-matching against the same variable.
      val (bindings, vVar) = expr match
        case v: Expr.Var => (Nil, v)
        case _ =>
          val v = freshVal()
          val vVar = Expr.Var(v)
          (List(Instr.Let(vVar, expr)), vVar)
      val ret = freshRet()
      bindings ++ List(
        Instr.IfChain(
          branches = List(
            (
              Cond.HasField(Expr.Field(vVar, "Type")),
              List(Instr.Return(Some(vVar))),
            ),
          ),
          fallback = List(
            Instr.Perform(
              "NormalCompletion",
              List(vVar),
              PerformOutcome.BindResult(ret),
            ),
            Instr.Return(Some(Expr.Var(ret))),
          ),
        ),
      )
    case Instr.Return(None, body) =>
      val ret = freshRet()
      List(
        Instr.Perform(
          "NormalCompletion",
          List(Expr.SpecTerm("unused")),
          PerformOutcome.BindResult(ret),
        ),
        Instr.Return(Some(Expr.Var(ret)), expand(body)),
      )
    case other =>
      List(other.mapBody(expand))
