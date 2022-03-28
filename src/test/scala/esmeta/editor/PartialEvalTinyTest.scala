package esmeta.editor

import esmeta.editor.sview.*

class PartialEvalTinyTest extends EditorTest {
  val name: String = "partialEvalParseTest"

  // registration
  def init: Unit =
    val peval = PartialEval(EditorTest.cfg)
    peval(EditorTest.parse("42 ?? #Identifier"))
  init

}
