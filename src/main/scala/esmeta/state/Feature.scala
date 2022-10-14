package esmeta.state

import esmeta.cfg.*
import esmeta.es.*
import esmeta.ir.Name
import esmeta.spec.*
import esmeta.ty.AstSingleTy

/** ECMAScript features */
sealed trait Feature extends StateElem {
  def func: Func
  def head: Head
}
case class SyntacticFeature(
  func: Func,
  head: SyntaxDirectedOperationHead,
) extends Feature
case class BuiltinFeature(
  func: Func,
  head: BuiltinHead,
) extends Feature
