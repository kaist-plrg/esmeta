package esmeta.es

import esmeta.ESMetaTest
import esmeta.analyzer.*
import esmeta.analyzer.domain.*
import esmeta.cfgBuilder.CFGBuilder
import esmeta.compiler.Compiler
import esmeta.error.NoGraal
import esmeta.es.util.injector.*
import esmeta.es.util.withCFG
import esmeta.interpreter.*
import esmeta.ir.NormalInst
import esmeta.parser.AstFrom
import esmeta.spec.Spec
import esmeta.state.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import org.scalatest.Assertions.*

/** ECMAScript test */
trait ESTest extends ESMetaTest {
  def category: String = "es"
}
object ESTest {
  import ESMetaTest.*

  // file extension converter from .js to .ir
  lazy val js2ir = changeExt("js", "ir")

  // ---------------------------------------------------------------------------
  // parser helpers
  // ---------------------------------------------------------------------------
  // parse ES codes
  lazy val scriptParser: AstFrom = spec.scriptParser
  def parse(str: String): Ast = scriptParser.from(str)
  def parseFile(filename: String): Ast = scriptParser.fromFile(filename)

  // ---------------------------------------------------------------------------
  // interpreter helpers
  // ---------------------------------------------------------------------------
  // interpreter with additional assertion checks
  class CheckAfter(
    st: State,
    checkAfter: List[NormalInst],
  ) extends Interpreter(st):
    override lazy val result: State =
      while (step) {}
      for (assert <- checkAfter) super.eval(assert)
      st

  // eval ES codes
  def eval(
    str: String,
    checkAfter: List[NormalInst] = Nil,
  ): State =
    new CheckAfter(cfg.init.from(str), checkAfter).result
  def evalFile(
    filename: String,
    checkAfter: List[NormalInst] = Nil,
  ): State = new CheckAfter(cfg.init.fromFile(filename), checkAfter).result

  // ---------------------------------------------------------------------------
  // analyzer helpers
  // ---------------------------------------------------------------------------
  // analyzer
  lazy val analyzer = ESAnalyzer(cfg)

  // analyze ES codes
  def analyzeFile(filename: String): AbsSemantics =
    analyzer(readFile(filename).trim)
  def analyze(str: String): AbsSemantics = analyzer(str)

  // tests for ES parser
  def parseTest(ast: Ast): Ast =
    val newAst = parse(ast.toString(grammar))
    assert(ast == newAst)
    ast
  def parseTest(str: String): Ast = parseTest(parse(str))
  def parseFileTest(filename: String): Ast = parseTest(parseFile(filename))

  // tests for ES interpreter
  def checkExit(st: State): st.type = st(GLOBAL_RESULT) match
    case Undef => st
    case v     => fail(s"return not undefined: $v")
  def evalTest(
    str: String,
    checkAfter: List[NormalInst] = Nil,
  ): State = checkExit(eval(str, checkAfter))
  def evalTestFile(
    filename: String,
    checkAfter: List[NormalInst] = Nil,
  ): State = checkExit(evalFile(filename, checkAfter))

  // tests for ES analyzer
  def checkExit(absSem: AbsSemantics): AbsSemantics =
    assert(absSem.finalResult.value.getSingle == One(Undef))
    absSem
  def analyzeTest(str: String): AbsSemantics = checkExit(analyze(str))
  def analyzeTestFile(filename: String): AbsSemantics =
    checkExit(analyzeFile(filename))

  // ---------------------------------------------------------------------------
  // trans-checker helpers
  // ---------------------------------------------------------------------------

  // tests for ES trans-checker
  def transCheckTest(str: String): Unit = withCFG(cfg) {
    try {
      val (origTest, transTest) = ConformTest.createTestPair(str)
      assert(origTest.isPass)
      assert(transTest.isPass)
    } catch { case NoGraal => }
  }
  def transCheckTestFile(filename: String): Unit =
    transCheckTest(readFile(filename))
}
