package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Cond, Expr, Instr}
import esmeta.wji.lang.Instr.PerformOutcome

/** Shared logic for wrapping every `Return`/`Throw` exit path of an algorithm's
  * body in an ECMA-262 Completion Record: converts a `Instr.Throw` into a real
  * `ThrowCompletion` return, and wraps every other `Instr.Return` in
  * `NormalCompletion` unless the value being returned is already itself a
  * completion (checked at runtime via `HasField`, mirroring mainline
  * `esmeta.compiler.Compiler`'s own `isCompletion(x)` guard on `ReturnStep`) —
  * so a caller can always assume the result has a `.Type` field.
  *
  * {{{
  *   If(cond, [Throw(New("TypeError"))])
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
  * `Instr.Throw`'s `target` is already a real `Expr` by the time this runs
  * (parsed at `InstrParser`, not here — see `Instr.Throw`'s own doc): an
  * `Expr.New(iface)` ("a {{Iface}} exception"/"a [=/new=] {{Iface}} object")
  * constructs a fresh error via `__NEW_ERROR_OBJ__` as before; anything else is
  * already a computed value (e.g. `ToJSValue(...)`, a bound variable) and gets
  * wrapped in `ThrowCompletion` directly, no construction step. Always emits
  * already-compiler-ready `Instr.Perform(..., BindResult(_))` + `Instr.Return`
  * — never an inline `Expr.AlgoCall` — so a caller never needs a later pass
  * (`ExpandInlineAlgoCallPass`/`ExpandPerformReturnResultPass`) to finish the
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
  *     closure unconditionally, regardless of that fixed point (mainline's own
  *     `esmeta.compiler.Compiler.compile`'s `Builtin => needRetComp = true` — a
  *     calling-convention requirement, not a "can it throw" one), for an
  *     `Algorithm` that didn't even exist yet when
  *     `WrapCompletionReturnsPass`'s own `algos.map` ran.
  */
object CompletionWrapping:

  private var counter = 0
  private def freshErr(): String = { counter += 1; s"_err$counter" }
  private def freshVal(): String = { counter += 1; s"_v$counter" }
  private def freshRet(): String = { counter += 1; s"_ret$counter" }

  /** Entry point for a caller's per-`Algorithm` pass over `expand`: resets
    * `counter` first so fresh names start over at each algorithm instead of
    * accumulating across the whole program (`expand` itself also recurses into
    * nested bodies, e.g. `Return(None, body)`'s `body` — those inner calls must
    * NOT reset the counter mid-algorithm, so the reset lives here rather than
    * inside `expand`).
    */
  def expandAlgorithm(instrs: List[Instr]): List[Instr] =
    counter = 0
    expand(instrs)

  def expand(instrs: List[Instr]): List[Instr] =
    instrs.flatMap(expandInstr)

  private def expandInstr(instr: Instr): List[Instr] = instr match
    // a "the following steps..." closure's substeps are a separate
    // algorithm-to-be (see CompletionAlgorithms) — its own Returns are none
    // of this algorithm's business to wrap. (A no-op by the time
    // AddBuiltinBehaviourPass calls this: no FollowingSteps remain anywhere
    // in the program past ExpandFollowingStepsPass.)
    case i @ Instr.Let(_, Expr.FollowingSteps(_, _), _) => List(i)
    case Instr.Throw(Expr.New(iface), _) =>
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
    // target is already a computed value (e.g. a ToJSValue(...) call, or a
    // bound variable holding an already-constructed exception object) --
    // unlike the Expr.New case above, there's nothing to construct, so wrap
    // it in ThrowCompletion directly.
    case Instr.Throw(target, _) =>
      val ret = freshRet()
      List(
        Instr.Perform(
          "ThrowCompletion",
          List(target),
          PerformOutcome.BindResult(ret),
        ),
        Instr.Return(Some(Expr.Var(ret))),
      )
    case Instr.Return(Some(expr), _) =>
      // a bare Var (the common case now that ExpandInlineAlgoCallPass/
      // ExpandPerformReturnResultPass already ran) needs no re-binding —
      // testing/returning it directly, rather than aliasing it under a fresh
      // name first, avoids a pointless extra Let.
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
              Cond.IsType(vVar, "Completion"),
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
