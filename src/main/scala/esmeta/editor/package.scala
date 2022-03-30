package esmeta.editor

import esmeta.editor.util.*
import esmeta.spec.Grammar
import esmeta.util.BaseUtils.*
import esmeta.error.AnalysisImpreciseError

/** editor elements */
trait EditorElem {
  override def toString: String = toString(true, false)

  /** stringify with options */
  def toString(
    detail: Boolean = true,
    location: Boolean = false,
    grammar: Option[Grammar] = None,
  ): String = {
    val stringifier = EditorElem.getStringifier(detail, location, grammar)
    import stringifier.elemRule
    stringify(this)
  }
}

object EditorElem {
  val getStringifier =
    cached[(Boolean, Boolean, Option[Grammar]), Stringifier] {
      new Stringifier(_, _, _)
    }
}

// Exploded
def exploded(msg: String = ""): Nothing = throw AnalysisImpreciseError(msg)
