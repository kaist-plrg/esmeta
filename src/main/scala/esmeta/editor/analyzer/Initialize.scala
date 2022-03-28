package esmeta.editor.analyzer

import esmeta.editor.sview.SyntacticView
import esmeta.cfg.{Node}
import esmeta.editor.util.CFGHelper

case class Initialize[AbsState](cfgHelper: CFGHelper) {
  def apply(view: SyntacticView): Map[NodePoint[Node], AbsState] = {
    Map()
  }
}
