package esmeta.editor.analyzer

import esmeta.cfg.Func
import esmeta.cfg.Node

sealed trait ControlPoint {
  val view: View

  def func: Func

}

case class NodePoint[+T <: Node](func: Func, node: T, view: View)
  extends ControlPoint {
  override def toString =
    s"(${func.name} ${node} ${view._1.length} ${view._2.length} ${view._3})"
}
case class ReturnPoint(func: Func, view: View) extends ControlPoint {
  override def toString =
    s"R(${func.name} ${view._1.length} ${view._2.length} ${view._3})"
}
