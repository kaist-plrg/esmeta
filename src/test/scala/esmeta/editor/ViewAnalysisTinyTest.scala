package esmeta.editor

import esmeta.editor.sview.*
import esmeta.editor.util.CFGHelper

class ViewAnalysisTinyTest extends EditorTest {
  val name: String = "partialEvalParseTest"

  // registration
  def init: Unit =
    check("viewanalysis") {
      val af = AnalysisFrontend(CFGHelper(EditorTest.cfg))
      af.cg(
        EditorTest.parse(
          "#MultiplicativeExpression# + #MultiplicativeExpression#",
        ),
      )
      af.cg(
        EditorTest.parse(
          "#RelationalExpression# == #RelationalExpression#",
        ),
      )
      af.cg(
        EditorTest.parse(
          "for ( var #ForBinding# in #Expression# ) #Statement#",
        ),
      )
    }
  init

}
