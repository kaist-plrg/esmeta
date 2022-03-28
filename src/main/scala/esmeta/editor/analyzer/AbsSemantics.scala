package esmeta.editor.analyzer

import esmeta.{DEBUG}
import esmeta.cfg.CFG
import esmeta.js.Ast
import esmeta.cfg.{Func, Node, Call}
import esmeta.util.BaseUtils.*
import esmeta.interp.State
import esmeta.editor.sview.*
import esmeta.editor.util.{Worklist, QueueWorklist}
import scala.annotation.tailrec
import esmeta.editor.util.CFGHelper
import esmeta.error.AnalysisTimeoutError

case class AbsSemantics[AbsState, AbsRet](
  val cfgHelper: CFGHelper,
  var npMap: Map[NodePoint[Node], AbsState] = Map(),
  var rpMap: Map[ReturnPoint, AbsRet] = Map(),
  var callInfo: Map[NodePoint[Call], AbsState] = Map(),
  var retEdges: Map[ReturnPoint, Set[NodePoint[Call]]] = Map(),
  var loopOut: Map[View, Set[View]] = Map(),
  timeLimit: Option[Long] = None,
) {

  // the number of iterations
  def getIter: Int = iter
  private var iter: Int = 0

  // set start time of analyzer
  val startTime: Long = System.currentTimeMillis

  // iteration period for check
  val CHECK_PERIOD = 10000

  // a worklist of control points
  val worklist: Worklist[ControlPoint] = new QueueWorklist(npMap.keySet)

  val transfer: AbsTransfer[AbsState, AbsRet] = AbsTransfer(this)

  // fixpiont computation
  @tailrec
  final def fixpoint: AbsSemantics[AbsState, AbsRet] = worklist.next match {
    case Some(cp) => {
      iter += 1

      // check time limit
      if (iter % CHECK_PERIOD == 0) timeLimit.map(limit => {
        val duration = (System.currentTimeMillis - startTime) / 1000
        if (duration > limit) throw AnalysisTimeoutError
      })

      // text-based debugging
      if (DEBUG) println(s"${cp.func.name}:$cp")

      // abstract transfer for the current control point
      transfer(cp)

      // keep going
      fixpoint
    }
    case None =>
      // final result
      this
  }
}
