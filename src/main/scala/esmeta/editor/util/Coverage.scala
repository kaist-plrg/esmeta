package esmeta.editor.util

import esmeta.error.*
import esmeta.interp.*
import esmeta.cfg.*
import esmeta.js.*

/** measure branch coverage of given JS program */
// TODO consider EReturnIfAbrupt
class Coverage(st: State, var covered: Set[(Int, Boolean)] = Set())
  extends Interp(st, Nil) {

  /** override transition for nodes for measure coverage */
  override def interp(node: Node): Unit = node match
    case Branch(id, _, cond, thenNode, elseNode) =>
      // evaluate cond and set cursor
      val b = interp(cond).escaped match
        case Bool(b) => b
        case v       => throw NoBoolean(cond, v)
      st.context.cursor = Cursor(if (b) thenNode else elseNode, st.func)
      covered += ((id, b)) // record
    case _ => super.interp(node)
}

object Coverage {
  def apply(cfg: CFG, sourceText: String): Set[(Int, Boolean)] =
    val st = Initialize(cfg, sourceText)
    val cov = new Coverage(st)
    cov.fixpoint
    cov.covered
}
