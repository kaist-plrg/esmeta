package esmeta.editor

class PartialEvalTinyTest extends EditorTest {
  val name: String = "partialEvalParseTest"

  // registration
  def init: Unit =
    val peval = PartialEval(EditorTest.cfg)
    peval(SyntacticView(EditorTest.parse("42 ?? #Identifier")))
  init

}
