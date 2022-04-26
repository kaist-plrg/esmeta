package esmeta.editor.analyzer

import esmeta.ir.Type

trait AbsObjDomain[AVD <: AbsValueDomain with Singleton](
  val avd: AVD,
) extends Domain {

  def apply(asiteTy: String): Elem

  type Elem <: AbsObjTrait

  trait AbsObjTrait extends ElemTrait { this: Elem =>
    def getType: Set[Type]
    def concat(that: Elem): Elem
  }
}
