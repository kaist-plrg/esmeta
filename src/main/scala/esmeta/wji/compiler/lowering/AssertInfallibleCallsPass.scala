package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Expr}
import esmeta.wji.lang.walker.Walker

/** *(hardcoding)* marks `!` on a few calls that cannot fail for the arguments
  * they are given, though the callee can fail for others. Whether an algorithm
  * returns a Completion Record is decided per callee, not per argument (see
  * [[CompletionAlgorithms]]), so without this every caller up the chain gets a
  * dead "if abrupt, propagate" guard (`docs/hardcodes.md` #28):
  *
  *   - `new` calls `internally create a new object implementing the interface`
  *     with newTarget `undefined`, which skips its only `?` steps.
  *   - `a new promise` and `react` call `? NewPromiseCapability` on the realm's
  *     own `%Promise%`, which ECMA-262 itself always calls with `!`.
  *
  * A `!` still asserts at run time, so a broken assumption fails loudly.
  *
  * Category: Completion-record convention.
  */
object AssertInfallibleCallsPass extends LoweringPass:

  override def requires: Set[LoweringPass] = Set(ResolveLinksPass)
  override def mustPrecede: Set[LoweringPass] = Set(
    MarkCompletionAlgorithmsPass,
  )

  /** algorithm name -> the callee whose call in it cannot fail */
  private val infallibleCalls: Map[String, String] = Map(
    "new" -> "internally create a new object implementing the interface",
    "a new promise" -> "newpromisecapability",
    "react" -> "newpromisecapability",
  )

  private def calleeName(link: String): String =
    link
      .stripPrefix("[=")
      .stripPrefix("[$")
      .stripSuffix("=]")
      .stripSuffix("$]")
      .trim
      .toLowerCase

  private class Asserter(callee: String) extends Walker:
    private def isTarget(e: Expr): Boolean = e match
      case Expr.AlgoCall(link, _) => calleeName(link) == callee
      case Expr.JSCall(name, _)   => calleeName(name) == callee
      case _                      => false

    override def walk(expr: Expr): Expr = expr match
      case Expr.Abrupt(_, inner) if isTarget(inner) =>
        Expr.Abrupt("!", super.walk(inner))
      case e if isTarget(e) => Expr.Abrupt("!", super.walk(e))
      case other            => super.walk(other)

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map { a =>
      a.name.map(_.toLowerCase).flatMap(infallibleCalls.get) match
        case Some(callee) =>
          val asserter = Asserter(callee)
          a.copy(body = a.body.map(asserter.walk))
        case None => a
    }
