package esmeta.editor.analyzer

trait AbsStateDomain[AVD <: AbsValueDomain with Singleton](
  val avd: AVD,
) extends Domain {

  type Elem <: AStateTrait

  trait AStateTrait extends ElemTrait { this: Elem =>
    def addId(id: Int, av: avd.Elem): Elem
  }

}
