package esmeta.editor

import esmeta.ESMetaTest
import esmeta.js.Ast
import esmeta.js.util.*

trait EditorTest extends ESMetaTest {
  def category: String = "editor"
}

object EditorTest {
  lazy val spec = ESMetaTest.spec
  lazy val grammar = spec.grammar
  lazy val cfg = spec.toCFG

  // parse JS codes
  lazy val parser = Parser(grammar)("Script")
  def parse(str: String): Ast = parser.from(str)

  // tests for JS parser
  def parseTest(ast: Ast): Ast =
    val newAst = parser.from(ast.toString(grammar = Some(grammar)))
    assert(ast == newAst)
    ast
  def parseTest(str: String): Ast = parseTest(parse(str))

}