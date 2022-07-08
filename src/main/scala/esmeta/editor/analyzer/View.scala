package esmeta.editor.analyzer

import esmeta.cfg.Branch
import esmeta.cfg.Call
import esmeta.editor.sview.SyntacticView

case class View(
  calls: List[Call] = Nil,
  loops: List[LoopCtxt] = Nil,
  intraLoopDepth: Int = 0,
  sviewOpt: Option[SyntacticView] = None,
) {

  // get ir ijk
  def getIrIJK: (Int, Int, Int) = (
    if (loops.isEmpty) 0 else loops.map(_.depth).max,
    loops.length,
    calls.length,
  )

  override def toString: String = 
    s"View($calls,$loops,$intraLoopDepth,${sviewOpt.map(v => v.name)})"
}

// contexts
case class LoopCtxt(loopId: Int, depth: Int)

// config
case class ViewConfig(maxLoopIter: Int, maxLoopDepth: Int, maxCallDepth: Int)
