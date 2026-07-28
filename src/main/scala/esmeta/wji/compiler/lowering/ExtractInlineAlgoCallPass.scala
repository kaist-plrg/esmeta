package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Expr, Instr}
import esmeta.wji.lang.Instr.PerformOutcome

/** Converts `AlgoCall`-with-args and `JSCall` (a `[$name$](...)` reference into
  * ECMA-262 itself, e.g. `[$NewPromiseCapability$](...)`) that appear in
  * expression position into explicit `Perform` statements so the compiler can
  * emit `ICall` instead of `EYet("call ...")`.
  *
  * Handles:
  * {{{
  *   Let(x, AlgoCall(f, args), body)                   → Perform(f, args, BindResult(x), body)
  *   Let(x, Abrupt("!", AlgoCall(f, args)), body)       → same  (strips ! wrapper)
  *   Return(Some(AlgoCall(f, args)), body)              → Perform(f, args, ReturnResult, body)
  *   Return(Some(Abrupt("!", AlgoCall(f, args))), body) → same
  *   (and likewise for JSCall(f, args) in each position)
  * }}}
  *
  * Zero-argument `AlgoCall`s in `Return` position are left alone (the compiler
  * emits `ERef(Global(name))` for those, which is already correct); `JSCall`
  * has no such zero-arg ambiguity and is always extracted.
  *
  * Category: Structural desugaring.
  */
object ExtractInlineAlgoCallPass extends LoweringPass:

  /** Requires:
    *   - [[ResolveLinksPass]]: matches `Expr.AlgoCall`/`Expr.JSCall`
    *     specifically — a raw `Expr.Link` is invisible to it.
    *   - [[ExpandAbruptPass]]: needs `Let(_tupleN, AlgoCall(...))` nodes
    *     already visible (no leftover `?`/`!` wrapper hiding the call).
    *   - [[ExpandDestructuringLetPass]]: same — needs destructuring already
    *     expanded so the `AlgoCall` sits directly in `Let`/`Return` RHS.
    *   - [[NormalizeEvaluationOrderPass]]: needs every nested call already
    *     hoisted out of a top-level `AlgoCall`'s own `args` first.
    */
  override def requires: Set[LoweringPass] = Set(
    ResolveLinksPass,
    ExpandAbruptPass,
    ExpandDestructuringLetPass,
    NormalizeEvaluationOrderPass,
  )

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = transform(a.body)))

  private def transform(instrs: List[Instr]): List[Instr] =
    instrs.flatMap(expandInstr)

  private def expandInstr(instr: Instr): List[Instr] = instr match

    case Instr.Let(Expr.Var(x), rhs, body) =>
      // include zero-arg AlgoCalls: "Let |p| be [=a new promise=]" is a call
      extractCall(rhs, zeroArg = true) match
        case Some((link, args)) =>
          List(
            Instr.Perform(
              link,
              args,
              PerformOutcome.BindResult(x),
              transform(body),
            ),
          )
        case None =>
          List(instr.mapBody(transform))

    case Instr.Return(Some(rhs), body) =>
      // zero-arg AlgoCalls in Return are spec value refs (e.g. [=error=]), not calls
      extractCall(rhs, zeroArg = false) match
        case Some((link, args)) =>
          List(
            Instr.Perform(
              link,
              args,
              PerformOutcome.ReturnResult,
              transform(body),
            ),
          )
        case None =>
          List(instr.mapBody(transform))

    case _ =>
      List(instr.mapBody(transform))

  /** Unwraps an optional `!` (assert-not-abrupt) wrapper and returns `(name,
    * args)` if the expression is an `AlgoCall` or a `JSCall`.
    *
    * @param zeroArg
    *   if true, zero-argument `AlgoCall`s are included (for Let RHS); if false,
    *   only `AlgoCall`s with arguments (for Return value). `JSCall` is always
    *   included regardless — its `[$name$](...)` syntax always carries an
    *   argument list, so (unlike a bare `[=link=]`) it is never ambiguous with
    *   a plain spec-term reference.
    */
  private def extractCall(
    expr: Expr,
    zeroArg: Boolean,
  ): Option[(String, List[Expr])] = expr match
    case Expr.AlgoCall(link, args) if zeroArg || args.nonEmpty =>
      Some((link, args))
    case Expr.Abrupt("!", Expr.AlgoCall(link, args))
        if zeroArg || args.nonEmpty =>
      Some((link, args))
    case Expr.JSCall(name, args) =>
      Some((name, args))
    case Expr.Abrupt("!", Expr.JSCall(name, args)) =>
      Some((name, args))
    case _ =>
      None
