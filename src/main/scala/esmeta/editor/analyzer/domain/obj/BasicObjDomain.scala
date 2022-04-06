package esmeta.editor.analyzer
import esmeta.util.Appender.*
import esmeta.util.Appender
import esmeta.ir.Type

class BasicObjDomain[AVD <: AbsValueDomain with Singleton](avd_ : AVD)
  extends AbsObjDomain[AVD](avd_) {
  val Bot = Elem()

  case class Elem() extends AbsObjTrait {
    def ⊑(that: Elem): Boolean = ???
    def ⊔(that: Elem): Elem = ???

    def getType: Type = ???
    def concat(that: Elem): Elem = this
  }

  // appender
  implicit val app: Rule[Elem] = (app, elem) => app >> ""
}
