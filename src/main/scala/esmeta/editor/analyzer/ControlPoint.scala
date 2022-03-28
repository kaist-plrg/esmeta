package esmeta.editor.analyzer

import esmeta.cfg.Func
import esmeta.cfg.Node

sealed trait ControlPoint {
  val view: View

  def func: Func

}

case class NodePoint[+T <: Node](func: Func, node: T, view: View)
  extends ControlPoint
case class ReturnPoint(func: Func, view: View) extends ControlPoint
