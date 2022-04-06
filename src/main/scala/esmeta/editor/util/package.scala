package esmeta.editor.util

import esmeta.error.*
import esmeta.interp.*
import esmeta.cfg.*
import esmeta.js.*
import esmeta.util.BaseUtils.*

/** measure statement coverage of given JS program */
def measureCoverage(
  cfg: CFG,
  sourceText: String,
  cachedAst: Option[Ast] = None,
  checkExit: Boolean = true,
): Set[Int] =
  val st = Initialize(cfg, sourceText, cachedAst)
  var touched: Set[Int] = Set()

  // run interp
  new Interp(st, Nil) {
    override def interp(node: Node): Unit =
      touched += node.id
      super.interp(node)
  }.fixpoint

  // check exit and return result
  if (checkExit) st(GLOBAL_RESULT) match
    case comp: Comp if comp.ty == CONST_NORMAL =>
    case v                                     => error(s"not normal exit")
  touched
