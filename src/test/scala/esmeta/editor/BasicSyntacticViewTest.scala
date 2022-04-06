package esmeta.editor

import esmeta.editor.sview.*
import esmeta.editor.util.CFGHelper

class BasicSyntacticViewTest extends EditorTest {
  val name: String = "basicSyntacticViewTest"

  // registration
  def init: Unit =
    check("basicSyntacticView") {
      val peval = PartialEval(CFGHelper(EditorTest.cfg))
      val viewSet = BasicSyntacticView(CFGHelper(EditorTest.cfg)).viewSet
      viewSet.foreach((v) => peval.apply(v))
    }
  init

}
