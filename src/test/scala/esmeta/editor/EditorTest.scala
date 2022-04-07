package esmeta.editor

import esmeta.ESMetaTest
import esmeta.editor.sview.*
import esmeta.editor.util.*
import esmeta.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*

trait EditorTest extends ESMetaTest {
  def category: String = "editor"

  // predefined data
  lazy val logDir = s"$LOG_DIR/editor_$dateStr"
}

object EditorTest {
  lazy val spec = ESMetaTest.spec
  lazy val grammar = spec.grammar
  lazy val cfg = spec.toCFG

  // parse JS codes
  lazy val parser = Parser(grammar)("Script")
  def parse(str: String): SyntacticView = parser.from(str)

  // tests for JS parser
  def parseTest(ast: SyntacticView): SyntacticView =
    val newAst = parser.from(ast.toString(grammar = Some(grammar)))
    assert(ast == newAst)
    ast
  def parseTest(str: String): SyntacticView = parseTest(parse(str))

}
