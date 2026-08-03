package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Expr, Instr}
import esmeta.wji.lang.Instr.PerformOutcome

/** Converts `AlgoCall` and `JSCall` (a `[$name$](...)` reference into ECMA-262
  * itself, e.g. `[$NewPromiseCapability$](...)`) that appear in expression
  * position into explicit `Perform` statements so the compiler can emit `ICall`
  * instead of `EYet("call ...")`.
  *
  * Handles:
  * {{{
  *   Let(x, AlgoCall(f, args), body)       → Perform(f, args, BindResult(x), body)
  *   Return(Some(AlgoCall(f, args)), body) → Perform(f, args, ReturnResult, body)
  *   (and likewise for JSCall(f, args) in each position)
  * }}}
  *
  * Category: Structural desugaring — Elimination.
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
      extractCall(rhs) match
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
      extractCall(rhs) match
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

  /** Returns `(name, args)` if the expression is an `AlgoCall` or a `JSCall`.
    */
  private def extractCall(expr: Expr): Option[(String, List[Expr])] = expr match
    case Expr.AlgoCall(link, args) => Some((link, args))
    case Expr.JSCall(name, args)   => Some((name, args))
    case _                         => None
