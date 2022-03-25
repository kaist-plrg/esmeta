package esmeta.editor

class ParseTinyTest extends EditorTest {
  val name: String = "editorParseTest"

  // registration
  def init: Unit =
      EditorTest.parseTest("42 ?? #Identifier")
      EditorTest.parseTest("#Identifier + #Identifier")
      EditorTest.parseTest("for (var #Identifier of [1, 2, 3] ) #Statement")
  
  init

}
