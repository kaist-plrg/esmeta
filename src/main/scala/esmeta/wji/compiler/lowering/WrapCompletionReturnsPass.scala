package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}
import esmeta.wji.lang.Instr.PerformOutcome

/** For every algorithm in `completionAlgos` (see [[CompletionAlgorithms]]) — a
  * definite or transitively-inferred completion-returning operation — converts
  * every `Instr.Throw` into a real `ThrowCompletion` return, and wraps every
  * other `Instr.Return` in `NormalCompletion` unless the value being returned
  * is already itself a completion (e.g. one already produced by `?`'s own
  * abrupt-propagation path — `ExpandAbruptPass`'s `Return(Some(tmpVar))` —
  * which must be returned as-is, not double-wrapped; checked at runtime via
  * `HasField`, mirroring mainline `esmeta.compiler.Compiler`'s own
  * `isCompletion(x)` guard on `ReturnStep`), so `Compiler` never has to
  * special-case a bare `Instr.Throw` at all.
  *
  * Per ECMA-262 convention, an operation that can abruptly complete must
  * consistently return a Completion Record on *every* exit path, not just the
  * throwing ones — a caller has no way to tell "raw value" and "normal
  * completion" apart otherwise. Both `NormalCompletion` and `ThrowCompletion`
  * are real ECMA-262 abstract operations already present in the merged CFG
  * (`WjiInterp` merges WJI's functions into the same `Program` as mainline's),
  * so this only needs to *call* them — not construct a completion record by
  * hand. Runs after `ExtractInlineAlgoCallPass`/`ExpandPerformReturnResultPass`
  * (see `requires` below), so unlike an ordinary spec-prose call this can't
  * lean on either of those to turn a call into a `Perform` for it — it emits
  * the fully-lowered `Instr.Perform(..., BindResult(_), ...)` + `Instr.Return`
  * pair itself. Building the thrown error object itself reuses mainline's own
  * `__NEW_ERROR_OBJ__` auxiliary function
  * (`esmeta.ir.package.AUX_NEW_ERROR_OBJ`) the exact same way
  * `esmeta.compiler.Compiler`'s `ThrowStep` case does, just spelled as a
  * `Perform` (`Compiler.compileInstr`'s generic non-embedding `Perform`
  * dispatch already produces the matching `EClo("__NEW_ERROR_OBJ__", Nil)` for
  * that name).
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
  * phrasing is left as `Instr.Unknown` rather than guessed at.
  *
  * Targets every `Algorithm` with `returnsCompletion = true` — see
  * [[MarkCompletionAlgorithmsPass]].
  *
  * Category: Completion-record convention.
  */
object WrapCompletionReturnsPass extends LoweringPass:

  /** Requires:
    *   - [[ExpandAbruptPass]]: needs a `Throw`/`Return`'s own `body`/`expr`
    *     already in their final shape (no leftover `?`/`!` markers to expand).
    *   - [[ExpandDestructuringLetPass]]: same — needs destructuring `Let`s
    *     already expanded before it inspects `body`/`expr`.
    *   - [[InsertFallthroughReturnPass]]: needs a real `Instr.Return` to wrap
    *     even for an algorithm that would otherwise just fall off the end.
    *   - [[MarkCompletionAlgorithmsPass]]: needs `returnsCompletion` already
    *     stamped onto every `Algorithm` to know which ones to target.
    */
  override def requires: Set[LoweringPass] = Set(
    ExpandAbruptPass,
    ExpandDestructuringLetPass,
    InsertFallthroughReturnPass,
    MarkCompletionAlgorithmsPass,
  )

  private var counter = 0
  private def freshErr(): String = { counter += 1; s"_err$counter" }
  private def freshVal(): String = { counter += 1; s"_v$counter" }
  private def freshRet(): String = { counter += 1; s"_ret$counter" }

  // "a {{TypeError}} exception" / "a {{TypeError}} exception." / "a {{TypeError}}"
  private val ThrowTarget =
    """(?si)^an?\s+\{\{([^}]+)\}\}(?:\s+exception)?\.?$""".r

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map { a =>
      if a.returnsCompletion then a.copy(body = expand(a.body)) else a
    }

  private def expand(instrs: List[Instr]): List[Instr] =
    instrs.flatMap(expandInstr)

  private def expandInstr(instr: Instr): List[Instr] = instr match
    // a "the following steps..." closure's substeps are a separate
    // algorithm-to-be (see CompletionAlgorithms) — its own Returns are none
    // of this algorithm's business to wrap.
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
