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
import esmeta.cfg.Branch

class AbsSemantics(
  val avd: AbsValueDomain,
  val asd: AbsStateDomain[avd.type],
  val ard: RetDomain[avd.type, asd.type],
)(
  val cfgHelper: CFGHelper,
  var npMap: Map[NodePoint[Node], asd.Elem] = Map(),
  var rpMap: Map[ReturnPoint, ard.Elem] = Map(),
  var callInfo: Map[NodePoint[Call], asd.Elem] = Map(),
  var retEdges: Map[ReturnPoint, Set[NodePoint[Call]]] = Map(),
  var loopOut: Map[View, Set[View]] = Map(),
  timeLimit: Option[Long] = None,
) {

  type AValue = avd.Elem
  type AState = asd.Elem
  type AReturn = ard.Elem
  val AValue: avd.type = avd
  val AState: asd.type = asd
  val AReturn: ard.type = ard

  // the number of iterations
  def getIter: Int = iter
  private var iter: Int = 0

  // set start time of analyzer
  val startTime: Long = System.currentTimeMillis

  // iteration period for check
  val CHECK_PERIOD = 10000

  // a worklist of control points
  val worklist: Worklist[ControlPoint] = new QueueWorklist(npMap.keySet)

  val transfer: AbsTransfer = AbsTransfer(this)

  // fixpiont computation
  @tailrec
  final def fixpoint: this.type = worklist.next match {
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

  def apply(np: NodePoint[Node]): AState =
    npMap.getOrElse(np, AState.Bot)
  def apply(rp: ReturnPoint): AReturn = rpMap.getOrElse(rp, AReturn.Bot)

  def loopNext(view: View): View = ???
  def loopEnter(view: View, loop: Branch): View = ???
  def loopExit(view: View): View = ???

  // update internal map
  def +=(pair: (NodePoint[Node], AState)): Boolean = true

  def initialize(view: SyntacticView): Map[NodePoint[Node], AState] = {
    Map()
  }
}
