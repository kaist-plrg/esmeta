package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Expr, Instr}

/** `find_a_builtin`'s own "Return (|builtinSetName|, |builtin|)."
  * (js-api/index.bs:1854) compiles `Expr.Tuple` straight to mainline
  * `ir.ETup`, whose `eval` (`Interpreter.scala`) unconditionally
  * `toAL`-converts every element — correct for a genuine Wasm-*value* tuple
  * (e.g. a `func_alloc` result's `(store, funcaddr)`, or this pass's sibling
  * [[AddBuiltinFunctionHostfuncPass]]'s own `Return (store, result)`), but
  * `(|builtinSetName|, |builtin|)` is a pure WJI-level pair — `builtinSetName`
  * a string, and `builtin` (since [[AddJsStringBuiltinsPass]] filled in
  * `get_the_builtins_for_a_builtin_set`'s table) now possibly containing a
  * `steps` *closure* at index 2, which can never cross that boundary (nor
  * needs to: `validate_an_import_for_builtins`, this result's only consumer,
  * reads just `maybeBuiltin[1][1]` — the `funcType` — never `steps`).
  *
  * Rewrites this one `Return`'s operand from `Expr.Tuple` to `Expr.List_`
  * instead — a plain heap list, `Index`/`Field` access (`maybeBuiltin[1]`
  * etc.) works identically either way (`Compiler`'s own `TupleProj`/`Index`
  * compile to a generic `Field(base, EMath/expr)`, dispatching on the
  * runtime value's actual shape — see that case's own comment). Scoped to
  * this exact algorithm/shape rather than changing `Expr.Tuple`'s general
  * compilation: `Expr.Tuple` is genuinely relied on elsewhere for real
  * Wasm-value tuples (this pass's own sibling above) and as a structurally-
  * comparable map key (`Interpreter.scala`'s exported-object cache,
  * `map[(tup objectkind objectaddr)]`) — broadening the fix risks either of
  * those.
  *
  * Runs alongside [[AddJsStringBuiltinsPass]] (no `requires` between them —
  * order doesn't matter, they touch different algorithms), for the same
  * "very early, before `ResolveLinksPass`" reason (though this pass doesn't
  * itself introduce anything needing link resolution, it must still run
  * before `NormalizeEvaluationOrderPass`/`ExpandInlineAlgoCallPass`/etc.,
  * i.e. before compilation proper, so simplest to keep it with the other
  * injection passes rather than reason about a later cutoff).
  *
  * Category: Structural desugaring — Elimination.
  */
object FixFindABuiltinReturnPass extends LoweringPass:

  private val TargetAlgoName = "find a builtin"

  private def fixReturn(instrs: List[Instr]): List[Instr] =
    instrs.map {
      case r @ Instr.Return(Some(Expr.Tuple(elems)), body) =>
        r.copy(expr = Some(Expr.List_(elems)), body = fixReturn(body))
      case i => i.mapBody(fixReturn)
    }

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map { a =>
      if a.name.contains(TargetAlgoName) then a.copy(body = fixReturn(a.body))
      else a
    }
