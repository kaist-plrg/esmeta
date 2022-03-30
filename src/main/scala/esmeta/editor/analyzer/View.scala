package esmeta.editor.analyzer

import esmeta.cfg.Branch
import esmeta.cfg.Call

case class View(
  calls: List[Call] = Nil,
  loops: List[LoopCtxt] = Nil,
  intraLoopDepth: Int = 0,
) {

  // get ir ijk
  def getIrIJK: (Int, Int, Int) = (
    if (loops.isEmpty) 0 else loops.map(_.depth).max,
    loops.length,
    calls.length,
  )
}

// contexts
case class LoopCtxt(loopId: Int, depth: Int)
