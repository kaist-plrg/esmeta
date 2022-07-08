package esmeta.editor.analyzer

import esmeta.{DEBUG}
import esmeta.editor.exploded
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
import esmeta.ir.{Id, Name}
import esmeta.editor.CallGraph

class AbsSemantics[ASD <: AbsStateDomain[_] with Singleton](
  val ard: RetDomain[ASD],
)(
  val cfgHelper: CFGHelper,
  var npMap: Map[NodePoint[Node], ard.asd.Elem] = Map(),
  var rpMap: Map[ReturnPoint, ard.Elem] = Map(),
  var callInfo: Map[NodePoint[Call], ard.asd.Elem] = Map(),
  var retEdges: Map[ReturnPoint, Set[NodePoint[Call]]] = Map(),
  var ignoreRetEdges: Map[String, Set[NodePoint[Call]]] = Map(),
  var loopOut: Map[View, Set[View]] = Map(),
  val maxIJK: ViewConfig = ViewConfig(0, 0, 0),
  timeLimit: Option[Long] = None,
  ignoreCond: Boolean = false,
) {

  type AbsValue = ard.asd.aod.avd.Elem
  type AbsState = ard.asd.Elem
  type AbsRet = ard.Elem
  val AbsValue: ard.asd.aod.avd.type = ard.asd.aod.avd
  val AbsState: ard.asd.type = ard.asd
  val AbsRet: ard.type = ard

  // the number of iterations
  def getIter: Int = iter
  private var iter: Int = 0

  // set start time of analyzer
  val startTime: Long = System.currentTimeMillis

  // iteration period for check
  val CHECK_PERIOD = 10000

  // a worklist of control points
  val worklist: Worklist[ControlPoint] = new QueueWorklist(npMap.keySet)

  val transfer: AbsTransfer[ASD, this.type] = AbsTransfer(this, ignoreCond)

  def getCG: CallGraph = new CallGraph {
    val funcs =
      npMap.keySet.map(_.func.name) ++ retEdges.keySet.map(_.func.name)
    val func_targets =
      retEdges.toList.foldLeft(Map[String, Set[String]]()) {
        case (m, (ReturnPoint(rf, _), s)) =>
          s.foldLeft(m) {
            case (m, NodePoint(cf, _, _)) =>
              m + (cf.name -> (m.get(cf.name).getOrElse(Set()) + rf.name))
          }
      }
    override def draw_func_targets =
      ignoreRetEdges.toList.foldLeft(func_targets) {
        case (m, (rs, s)) =>
          s.foldLeft(m) {
            case (m, NodePoint(cf, _, _)) =>
              m + (cf.name -> (m.get(cf.name).getOrElse(Set()) + rs))
          }
      }
  }
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

  // get return edges
  def getRetEdges(rp: ReturnPoint): Set[NodePoint[Call]] =
    retEdges.getOrElse(rp, Set())

  def apply(np: NodePoint[Node]): AbsState =
    npMap.getOrElse(np, AbsState.Bot)
  def apply(rp: ReturnPoint): AbsRet = rpMap.getOrElse(rp, AbsRet.Bot)

  // update internal map
  def +=(pair: (NodePoint[Node], AbsState)): Boolean = {
    val (np, newSt) = pair
    val oldSt = this(np)
    val rv = if (!(newSt ⊑ oldSt)) {
      npMap += np -> (oldSt ⊔ newSt)
      worklist += np
      true
    } else false
    rv
  }

  // handle calls
  def doCall(
    call: Call,
    callerView: View,
    callerSt: AbsState,
    func: Func,
    st: AbsState,
    isSDO: Boolean = false,
  ): Unit = {
    // println("E")
    // println(func.toString)
    val callerNp = NodePoint(cfgHelper.cfg.funcOf(call), call, callerView)
    this.callInfo += callerNp -> callerSt

    val calleeView = viewCall(callerView, call, st, isSDO)
    func.entry.map(entry =>
      this += NodePoint(func, entry, calleeView) -> st.doCall,
    )

    val rp = ReturnPoint(func, calleeView)
    val set = retEdges.getOrElse(rp, Set())
    retEdges += rp -> (set + callerNp)

    val retT = this(rp)
    if (!retT.isBottom) worklist += rp
  }

  // call transition
  def viewCall(
    callerView: View,
    call: Call,
    st: AbsState,
    isSDO: Boolean,
  ): View = {
    val View(calls, _, _, _) = callerView
    val view = callerView.copy(
      calls = (call :: calls).take(maxIJK.maxCallDepth),
      intraLoopDepth = 0,
      sviewOpt = if (isSDO) getThis(st) else callerView.sviewOpt
    )
    view
  }

  // update return points
  def doReturn(rp: ReturnPoint, newRet: AbsRet): Unit = {
    val ReturnPoint(func, view) = rp
    val retRp = ReturnPoint(func, getEntryView(view))
    if (!newRet.value.isBottom) {
      val oldRet = this(retRp)
      if (newRet !⊑ oldRet) {
        rpMap += retRp -> (oldRet ⊔ newRet)
        worklist += retRp
      }
    }
  }

  // loop transition
  def loopNext(view: View): View = view.loops match {
    case LoopCtxt(loop, k) :: rest =>
      view.copy(loops = LoopCtxt(loop, (k + 1) min maxIJK.maxLoopIter) :: rest)
    case _ => view
  }
  def loopEnter(view: View, loop: Branch): View = {
    val loopView = view.copy(
      loops = (LoopCtxt(loop.id, 0) :: view.loops).take(maxIJK.maxLoopDepth),
      intraLoopDepth = view.intraLoopDepth + 1,
    )
    loopOut += loopView -> (loopOut.getOrElse(loopView, Set()) + view)
    loopView
  }
  def loopBase(view: View): View = view.loops match {
    case LoopCtxt(loop, k) :: rest =>
      view.copy(loops = LoopCtxt(loop, 0) :: rest)
    case _ => view
  }
  def loopExit(view: View): View = {
    val views = loopOut.getOrElse(loopBase(view), Set())
    views.size match {
      case 0 => ???
      case 1 => views.head
      case _ => exploded("loop is too merged.")
    }
  }

  // get entry views of loops
  @tailrec
  final def getEntryView(view: View): View = {
    if (view.intraLoopDepth == 0) view
    else getEntryView(loopExit(view))
  }

  // get abstract state of control points
  def getState(cp: ControlPoint): AbsState = cp match {
    case np: NodePoint[_] => this(np)
    case rp: ReturnPoint  => this(rp).state
  }

  def initialize(view: SyntacticView): Unit = {
    cfgHelper.getSDOView(view, "Evaluation") match {
      case Some((s, f)) =>
        f.entry
          .foreach(n =>
            this +=
              NodePoint(f, n, View(sviewOpt = Some(view))) -> AbsState.Empty.update(
                f.irFunc.params.head.lhs,
                sviewToAbsValue(s),
              ),
          )
      case None => ()
    }
  }

  def sviewToAbsValue(view: SyntacticView): AbsValue = AbsValue(
    AbsValue.ASView(view),
  )
  def absValueToSview(absValue: AbsValue): Option[SyntacticView] = {
    absValue.getSingle() match {
      case FlatElem(AbsValue.ASView(view)) => Some(view)
      case _ => None
    }
  }
  
  def getThis(st: AbsState): Option[SyntacticView] = absValueToSview(st.directLookup(Name("this")))
}
