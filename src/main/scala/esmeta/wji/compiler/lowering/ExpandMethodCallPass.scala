package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Expr}
import esmeta.wji.lang.walker.Walker
import esmeta.error.UnsupportedSpecShape

/** Makes an internal method call's implicit receiver explicit, turning
  * `Expr.MethodCall` into the `Expr.ClosureCall` every later pass already
  * handles:
  * {{{
  *   MethodCall(base, name, args)
  * }}}
  * becomes
  * {{{
  *   ClosureCall(Field(base, name), base :: args)
  * }}}
  * — the same thing mainline's `Compiler` does for an
  * `InvokeMethodExpression` (`call %0 = O.GetOwnProperty(O, P)`), and what
  * every internal method's own definition expects (its receiver is always its
  * first parameter, see `AlgorithmExtractor.InternalMethodSignature`).
  *
  * `base` appears twice in the result, so it must be a plain variable — the
  * only receiver ECMA-262 or WebIDL ever writes a method call on. Anything
  * else throws `UnsupportedSpecShape` rather than being silently evaluated
  * twice.
  *
  * Category: Structural desugaring — Elimination.
  */
object ExpandMethodCallPass extends LoweringPass:

  /** Must precede:
    *   - [[ExpandTryPass]]: recognizes a `Try` wrapping a call only in its
    *     `Expr.ClosureCall` shape.
    *   - [[ExpandClosureCallPass]]: the `Expr.ClosureCall` this pass produces
    *     must still be turned into a real `Instr.PerformClosure` — nothing
    *     later knows `Expr.MethodCall` at all.
    */
  override def mustPrecede: Set[LoweringPass] =
    Set(ExpandTryPass, ExpandClosureCallPass)

  override def postconditions: List[Condition] = List(
    Condition(
      "no Expr.MethodCall remains anywhere",
      algos =>
        !algos.exists(a =>
          AstQuery.existsExpr(a.body)(_.isInstanceOf[Expr.MethodCall]),
        ),
    ),
  )

  private object rewriter extends Walker:
    override def walk(expr: Expr): Expr = expr match
      case Expr.MethodCall(base: Expr.Var, name, args) =>
        Expr.ClosureCall(Expr.Field(base, name), base :: args.map(walk))
      case Expr.MethodCall(base, name, _) =>
        throw UnsupportedSpecShape(
          "ExpandMethodCallPass",
          s"method call [[$name]] on a receiver that is not a variable: $base",
        )
      case other => super.walk(other)

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = a.body.map(rewriter.walk)))
