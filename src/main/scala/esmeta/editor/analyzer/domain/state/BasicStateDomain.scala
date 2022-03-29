package esmeta.editor.analyzer

import esmeta.editor.analyzer.*
import esmeta.util.Appender.*
import esmeta.util.Appender

class BasicStateDomain[AVD <: AbsValueDomain with Singleton](avd_ : AVD)
  extends AbsStateDomain[AVD](avd_) {
  val Bot = Elem(
    value = avd.Bot,
  )

  case class Elem(value: avd.Elem) extends AStateTrait {

    def addId(id: Int, av: avd.Elem) = Elem(av)
    def ⊑(that: Elem): Boolean =
      this.value ⊑ that.value

    // join operator
    def ⊔(that: Elem): Elem = Elem(
      this.value ⊔ that.value,
    )

    // conversion to string
    def toString(detail: Boolean): String =
      val app = new Appender
      app >> this
      app.toString

  }

  // constructors
  def apply(
    value: avd.Elem = avd.Bot,
  ): Elem = Elem(value)

  // extractors
  def unapply(elem: Elem) = Some(
    (
      elem.value,
    ),
  )

  // appender
  implicit val app: Rule[Elem] = (app, elem) => app >> elem.value

}
