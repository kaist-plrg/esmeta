package esmeta.editor.analyzer

import esmeta.ir.*
import esmeta.cfg.*
import esmeta.interp.Interp
import esmeta.ir.{Func => IRFunc}

class AbsTransfer(val sem: AbsSemantics) {

  type AValue = sem.avd.Elem
  type AState = sem.asd.Elem
  type AReturn = sem.ard.Elem
  val AValue: sem.avd.type = sem.avd
  val AState: sem.asd.type = sem.asd
  val AReturn: sem.ard.type = sem.ard

  def apply(cp: ControlPoint): Unit = cp match {
    case (np: NodePoint[_]) => apply(np)
    case (rp: ReturnPoint)  => apply(rp)
  }

  def apply[T <: Node](np: NodePoint[T]): Unit = {
    val st = sem(np)
    val NodePoint(func, node, view) = np
    node match {
      case block @ Block(_, insts, next) =>
        val newSt = insts.foldLeft(st) {
          case (st, inst) => transfer(inst)(st)
        }
        next.foreach((next: Node) => sem += getNextNp(np, next) -> newSt)
      case call @ Call(_, _, _, _, next) =>
        val newSt = transfer(call)(st)
        next.foreach((next: Node) => sem += getNextNp(np, next) -> newSt)
      case Branch(id, kind, cond, thenNode, elseNode) =>
        val (v, newSt1) = escape(transfer(cond)(st))
        if (v containsBool true)
          thenNode.map((thenNode: Node) =>
            sem += getNextNp(np, thenNode) -> newSt1,
          )
        if (v containsBool false)
          elseNode.map((elseNode: Node) =>
            sem += getNextNp(np, elseNode, true) -> newSt1,
          )
    }
  }

  def apply(rp: ReturnPoint): Unit = {
    ()
  }

  def transfer(inst: Inst)(st: AState): AState = ???
  def transfer(call: Call)(st: AState): AState = ???
  def transfer(expr: Expr)(st: AState): (AValue, AState) = ???

  def escape(v: (AValue, AState)): (AValue, AState) = ???

  // get next node points (intra)
  def getNextNp(
    fromCp: NodePoint[Node],
    to: Node,
    loopOut: Boolean = false,
  ): NodePoint[Node] = {
    val NodePoint(func, from, view) = fromCp
    val toView = (from, to) match {
      case (
            Branch(id1, Branch.Kind.Loop(_), _, _, _),
            loop @ Branch(id2, Branch.Kind.Loop(_), _, _, _),
          ) =>
        if (id1 == id2) sem.loopNext(view)
        else if (loopOut) sem.loopExit(view)
        else sem.loopEnter(view, loop)
      case (_, loop @ Branch(_, Branch.Kind.Loop(_), _, _, _)) =>
        sem.loopEnter(view, loop)
      case (Branch(_, Branch.Kind.Loop(_), _, _, _), _) if loopOut =>
        sem.loopExit(view)
      case _ => view
    }
    NodePoint(func, to, toView)
  }

}
