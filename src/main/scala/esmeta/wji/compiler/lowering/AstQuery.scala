package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Expr, Instr}
import esmeta.wji.lang.util.UnitWalker

/** Generic read-only queries over an already-(partially-)lowered [[Instr]]
  * tree, shared by lowering passes that only handle a construct in a
  * documented-narrow set of positions and need to check, once they're done,
  * whether an occurrence outside that set slipped through — see
  * [[ExpandNewByteSequencePass]], [[ExpandIndexOfPass]], and
  * [[ExpandFollowingStepsPass]], which throw
  * `esmeta.error.UnsupportedSpecShape` when [[existsExpr]] finds one, rather
  * than silently leaving it for `Compiler`'s own, much later `EYet` fallback.
  */
object AstQuery:

  private class ExistsWalker(pred: Expr => Boolean) extends UnitWalker:
    var found = false
    override def walk(expr: Expr): Unit =
      if pred(expr) then found = true
      super.walk(expr)

  /** Whether `pred` holds for any `Expr` reachable from `instrs`, at any depth
    * — every instruction's own direct fields (including inside its `Cond`s),
    * plus every nested instruction body, via [[UnitWalker]]'s own exhaustive
    * default recursion.
    */
  def existsExpr(instrs: List[Instr])(pred: Expr => Boolean): Boolean =
    val w = ExistsWalker(pred)
    instrs.foreach(w.walk)
    w.found
