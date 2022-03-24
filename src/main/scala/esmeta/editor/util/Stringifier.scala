package esmeta.editor.util

import esmeta.LINE_SEP
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*
import esmeta.editor.*

/** stringifier for editor */
class Stringifier(detail: Boolean, location: Boolean) {
  // Editor elements
  given elemRule: Rule[EditorElem] = (app, elem) =>
    elem match {
      case view: SyntacticView => syntacticViewRule(app, view)
    }

  // TODO syntactic views
  given syntacticViewRule: Rule[SyntacticView] = (app, view) => {
    ???
  }
}
