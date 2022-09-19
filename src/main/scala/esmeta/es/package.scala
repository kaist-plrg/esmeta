package esmeta.es

import esmeta.es.util.*
import esmeta.spec.Grammar
import esmeta.util.BaseUtils.*

/** ECMAScript elements */
trait ESElem {
  override def toString: String = toString()

  /** stringify with grammar */
  def toString(
    grammar: Grammar,
  ): String = {
    val stringifier = ESElem.getStringifier(true, false, Some(grammar))
    import stringifier.elemRule
    stringify(this)
  }

  /** stringify with options but without grammar */
  def toString(
    detail: Boolean = true,
    location: Boolean = false,
  ): String = {
    val stringifier = ESElem.getStringifier(detail, location, None)
    import stringifier.elemRule
    stringify(this)
  }
}
object ESElem {
  val getStringifier =
    cached[(Boolean, Boolean, Option[Grammar]), Stringifier] {
      Stringifier(_, _, _)
    }
}
