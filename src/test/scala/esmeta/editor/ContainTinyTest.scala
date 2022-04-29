package esmeta.editor

import esmeta.ESMetaTest
import esmeta.editor.sview.*
import esmeta.editor.util.*
import esmeta.js.util.{Parser => JsParser}
import esmeta.js.{Ast, Syntactic => JsSyntactic}

class ContainTinyTest extends EditorTest {
  val name: String = "containTest"
  import EditorTest.*

  // parse ast
  lazy val jsParser = JsParser(grammar)("Script")
  def parseJs(str: String): Ast = jsParser.from(str)

  // get syntactic view
  def getSyntacticView(sviewStr: String): SyntacticView = {
    // TODO find actual root of syntactic view
    def getRoot(sv: SyntacticView): SyntacticView = sv match
      case Syntactic(_, _, _, List(Some(child))) => getRoot(child)
      case _                                     => sv
    getRoot(parse(sviewStr))
  }

  // get ast root
  def parseAndTrimJs(jsStr: String): Ast = {
    // TODO find actual root of ast
    def getRoot(ast: Ast): Ast = ast match
      case JsSyntactic(_, _, _, List(Some(child))) => getRoot(child)
      case _                                       => ast
    getRoot(parseJs(jsStr))
  }

  // check contains
  def containTest(
    jsStr: String,
    sviewStr: String,
    expected: Boolean = true,
  ): Unit =
    val (ast, sview) = (parseJs(jsStr), getSyntacticView(sviewStr))
    assert(ast.contains(sview) == expected)

  // check matches after simplify
  def simplifyAndMatchTest(
    jsStr: String,
    sviewStr: String,
    expected: Boolean = true,
  ): Unit =
    val (ast, sview) = (parseJs(jsStr), parse(sviewStr))
    assert(ast.matches(sview) == expected)
    val (simpleAst, simpleSview) = (ast.simplify(cfg), sview.simplify(cfg))
    val res = simpleAst.matches(simpleSview, cfg) == expected
    assert(res)

  // registration
  def init: Unit = {
    check("contains") {
      containTest(
        """
        |var acc = 0;
        |for ( var x in [1,2,3] ) { acc += x; }""".stripMargin,
        "for ( var #ForBinding# in #Expression# ) #Statement#",
      )

      containTest(
        """
        |var acc = 0;
        |for ( var x in [1,2,3] ) { acc += x; }""".stripMargin,
        "for ( var #BindingPattern# in #Expression# ) #Statement#",
        expected = false,
      )

      containTest(
        "x == x",
        "#RelationalExpression# == #RelationalExpression#",
      )

      containTest(
        "async function f() { var x; }",
        "var #BindingIdentifier#;",
      )

      containTest(
        """
        |function f () {}
        |async function f () {}
        |async function* f () {}
        |function* f () {}""".stripMargin,
        """
        |#FunctionDeclaration#
        |#AsyncFunctionDeclaration#
        |#AsyncGeneratorDeclaration#
        |#GeneratorDeclaration#""".stripMargin,
      )
    }

    check("matches after simplify") {
      simplifyAndMatchTest(
        "{ acc += x; }".stripMargin,
        "#Statement#",
      )

      simplifyAndMatchTest(
        "for ( var x in [1,2,3] ) { acc += x; }".stripMargin,
        "for ( var #ForBinding# in #Expression# ) #Statement#",
      )

      simplifyAndMatchTest(
        "for ( var x in [1,2,3] ) { acc += x; }".stripMargin,
        "for ( var #ForBinding# in #Expression# ) #Statement#",
      )

      simplifyAndMatchTest(
        "for ( var x in [1,2,3] ) { acc += x; }".stripMargin,
        "for ( var #BindingPattern# in #Expression# ) #Statement#",
        expected = false,
      )

      simplifyAndMatchTest(
        "x == x",
        "#RelationalExpression# == #RelationalExpression#",
      )

      simplifyAndMatchTest(
        "async function f() { var x; }",
        "async function f() { var #BindingIdentifier#; }",
      )
    }
  }
  init
}
