package esmeta.editor

import esmeta.editor.sview.*
import esmeta.editor.util.CFGHelper

class PartialEvalTinyTest extends EditorTest {
  val name: String = "partialEvalParseTest"

  // registration
  def init: Unit =
    check("peval") {
      val peval = PartialEval(CFGHelper(EditorTest.cfg))
      peval(
        EditorTest.parse(
          "#MultiplicativeExpression + #MultiplicativeExpression",
        ),
      )
      peval(
        EditorTest.parse(
          "#RelationalExpression == #RelationalExpression",
        ),
      )
      peval(
        EditorTest.parse(
          "for ( var #ForBinding in #Expression ) #Statement",
        ),
      )
    }
  init

}
