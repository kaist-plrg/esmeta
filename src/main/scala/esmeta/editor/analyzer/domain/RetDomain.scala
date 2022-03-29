package esmeta.editor.analyzer
import esmeta.util.Appender.*
import esmeta.util.Appender

class RetDomain[
  AVD <: AbsValueDomain with Singleton,
  ASD <: AbsStateDomain[AVD] with Singleton,
](val avd: AVD, val asd: ASD)
  extends Domain {
  val Bot = Elem(
    value = avd.Bot,
    state = asd.Bot,
  )

  // constructors
  def apply(
    value: avd.Elem = avd.Bot,
    state: asd.Elem = asd.Bot,
  ): Elem = Elem(value, state)

  // extractors
  def unapply(elem: Elem) = Some(
    (
      elem.value,
      elem.state,
    ),
  )

  // appender
  implicit val app: Rule[Elem] = (app, elem) => app >> elem.value

  // elements
  case class Elem(
    value: avd.Elem,
    state: asd.Elem,
  ) extends ElemTrait {
    // partial order
    def ⊑(that: Elem): Boolean = (
      this.value ⊑ that.value &&
        this.state ⊑ that.state
    )

    // join operator
    def ⊔(that: Elem): Elem = Elem(
      this.value ⊔ that.value,
      this.state ⊔ that.state,
    )

    // conversion to string
    def toString(detail: Boolean): String =
      val app = new Appender
      app >> this
      app.toString

  }
}
