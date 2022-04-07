package esmeta.editor

import esmeta.ESMetaTest
import esmeta.editor.sview.*
import esmeta.editor.util.*
import esmeta.js.util.{Parser => JsParser}
import esmeta.js.Ast

class ContainTinyTest extends EditorTest {
  val name: String = "containTest"
  import EditorTest.*

  // parse ast
  lazy val jsParser = JsParser(grammar)("Script")
  def parseJs(str: String): Ast = jsParser.from(str)

  // check contains
  def containTest(
    jsStr: String,
    sviewStr: String,
    expected: Boolean = true,
  ): Unit =
    val (ast, sview) = (parseJs(jsStr), parse(sviewStr))
    assert(ast.contains(sview) == expected)

  // registration
  def init: Unit = {
    check("contains") {
      containTest(
        """
        |var acc = 0;
        |for ( var x in [1,2,3] ) { acc += x; }""".stripMargin,
        "for ( var #ForBinding in #Expression ) #Statement",
      )

      containTest(
        """
        |var acc = 0;
        |for ( var x in [1,2,3] ) { acc += x; }""".stripMargin,
        "for ( var #BindingPattern in #Expression ) #Statement",
        expected = false,
      )

      containTest(
        "x == x",
        "#RelationalExpression == #RelationalExpression",
      )

      containTest(
        "async function f() { var x; }",
        "var #BindingIdentifier;",
      )

      containTest(
        """
        |function f () {}
        |async function f () {}
        |async function* f () {}
        |function* f () {}""".stripMargin,
        """
        |#FunctionDeclaration
        |#AsyncFunctionDeclaration
        |#AsyncGeneratorDeclaration
        |#GeneratorDeclaration""".stripMargin,
      )
    }
  }
  init
}
