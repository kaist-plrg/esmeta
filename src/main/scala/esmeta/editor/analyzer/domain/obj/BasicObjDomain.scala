package esmeta.editor.analyzer
import esmeta.util.Appender.*
import esmeta.util.Appender
import esmeta.ir.Type

class BasicObjDomain[AVD <: AbsValueDomain with Singleton](avd_ : AVD)
  extends AbsObjDomain[AVD](avd_) {

  def apply(ty: String) = Elem(Set(ty))
  val Bot = Elem(Set())
  val TopOpt = None

  case class Elem(ty: Set[String]) extends AbsObjTrait {
    def ⊑(that: Elem): Boolean = this.ty.forall(that.ty contains _)
    def ⊔(that: Elem): Elem = Elem(this.ty ++ that.ty)

    def getType: Set[Type] = ty.map(Type(_))
    def concat(that: Elem): Elem = this
  }

  // appender
  implicit val app: Rule[Elem] = (app, elem) => app >> ""
}
