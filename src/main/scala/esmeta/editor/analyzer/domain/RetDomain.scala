package esmeta.editor.analyzer
import esmeta.util.Appender.*
import esmeta.util.Appender

class RetDomain[
  ASD <: AbsStateDomain[_] with Singleton,
](val asd: ASD)
  extends Domain {

  val AbsValue: asd.aod.avd.type = asd.aod.avd
  type AbsValue = AbsValue.Elem

  val AbsState: asd.type = asd
  type AbsState = AbsState.Elem

  val Bot = Elem(
    value = asd.aod.avd.Bot,
    state = asd.Bot,
  )

  // constructors
  def apply(
    value: AbsValue = AbsValue.Bot,
    state: AbsState = AbsState.Bot,
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
    value: AbsValue,
    state: AbsState,
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
