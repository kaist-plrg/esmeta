package esmeta.editor.analyzer

import esmeta.util.Appender.*
import esmeta.util.Appender

class BasicValueDomain() extends AbsValueDomain {
  val Bot = Elem(
  )

  case class Elem() extends AValueTrait {

    def containsBool(b: Boolean) = true
    def ⊑(that: Elem): Boolean = (true)

    // join operator
    def ⊔(that: Elem): Elem = Elem(
    )

    // conversion to string
    def toString(detail: Boolean): String = {
      val app = new Appender
      app >> this
      app.toString
    }
  }

  // constructors
  def apply(
  ): Elem = Elem()

  // extractors
  def unapply(elem: Elem) = Some(
    (
    ),
  )

  // appender
  implicit val app: Rule[Elem] = (app, elem) => app >> ""

}
