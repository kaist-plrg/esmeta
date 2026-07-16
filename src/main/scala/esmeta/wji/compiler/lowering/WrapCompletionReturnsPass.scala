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
  * so this only needs to *call* them — via the ordinary `Expr.AlgoCall` +
  * `ExtractInlineAlgoCallPass` machinery already in the pipeline — not
  * construct a completion record by hand. Building the thrown error object
  * itself reuses mainline's own `__NEW_ERROR_OBJ__` auxiliary function
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
  *     Return(Some(AlgoCall("ThrowCompletion", [Var(_err1)]))),
  *   ])
  *   Let(_v1, expr)
  *   If(_v1 has field "Type", [Return(Some(_v1))])
  *   Else [Return(Some(AlgoCall("NormalCompletion", [_v1])))]
  * }}}
  *
  * Only recognizes a `Throw` target of the exact `"a {{Iface}}"` / `"a
  * {{Iface}} exception"` shape (every occurrence reached so far); any other
  * phrasing is left as `Instr.Unknown` rather than guessed at.
  *
  * Must run after [[ExpandAbruptPass]]/[[ExpandDestructuringLetPass]] (so a
  * `Throw`/`Return`'s own `body`/`expr` are already in their final shape) and
  * before [[ExtractInlineAlgoCallPass]] (which hoists the `AlgoCall`s this pass
  * produces into real `Perform`s).
  *
  * Targets every `Algorithm` with `returnsCompletion = true` — see
  * [[MarkCompletionAlgorithmsPass]], which must run earlier in the pipeline to
  * compute and stamp that field on.
  */
object WrapCompletionReturnsPass extends LoweringPass:

  private var counter = 0
  private def freshErr(): String = { counter += 1; s"_err$counter" }
  private def freshVal(): String = { counter += 1; s"_v$counter" }

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
      List(
        Instr.Perform(
          "__NEW_ERROR_OBJ__",
          List(Expr.Str(s"%$iface.prototype%")),
          PerformOutcome.BindResult(err),
        ),
        Instr.Return(
          Some(Expr.AlgoCall("ThrowCompletion", List(Expr.Var(err)))),
        ),
      )
    case t: Instr.Throw =>
      List(Instr.Unknown(s"throw ${t.target}"))
    case Instr.Return(Some(expr), _) =>
      val v = freshVal()
      val vVar = Expr.Var(v)
      List(
        Instr.Let(vVar, expr),
        Instr.IfChain(
          branches = List(
            (
              Cond.HasField(Expr.Field(vVar, "Type")),
              List(Instr.Return(Some(vVar))),
            ),
          ),
          fallback = List(
            Instr.Return(Some(Expr.AlgoCall("NormalCompletion", List(vVar)))),
          ),
        ),
      )
    case Instr.Return(None, body) =>
      List(
        Instr.Return(
          Some(
            Expr.AlgoCall("NormalCompletion", List(Expr.SpecTerm("unused"))),
          ),
          expand(body),
        ),
      )
    case other =>
      List(other.mapBody(expand))
