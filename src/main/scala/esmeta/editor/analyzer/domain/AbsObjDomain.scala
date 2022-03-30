package esmeta.editor.analyzer

import esmeta.ir.Type

trait AbsObjDomain[AVD <: AbsValueDomain with Singleton](
  val avd: AVD,
) extends Domain {

  type Elem <: AbsObjTrait

  trait AbsObjTrait extends ElemTrait { this: Elem =>
    def getType: Type
  }
}
