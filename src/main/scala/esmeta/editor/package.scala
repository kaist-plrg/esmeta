package esmeta.editor

import esmeta.editor.util.*
import esmeta.util.BaseUtils.*

/** editor elements */
trait EditorElem {
  override def toString: String = toString(true, false)

  /** stringify with options */
  def toString(
    detail: Boolean = true,
    location: Boolean = false,
  ): String = {
    val stringifier = EditorElem.getStringifier((detail, location))
    import stringifier.elemRule
    stringify(this)
  }
}

object EditorElem {
  val getStringifier = {
    cached[(Boolean, Boolean), Stringifier](
      new Stringifier(_, _),
    )
  }
}
