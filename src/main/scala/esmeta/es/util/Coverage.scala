package esmeta.es.util

import esmeta.cfg.*
import esmeta.es.*
import esmeta.interpreter.*
import esmeta.ir.{Expr, EParse}
import esmeta.state.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import io.circe.*, io.circe.syntax.*

/** coverage measurement of cfg */
class Coverage(
  cfg: CFG,
  timeLimit: Option[Int] = None,
) {

  // mapping from
  private var nodeMap: Map[Node, Script] = Map()
  private var scriptMap: Map[String, Script] = Map()
  private var codeMap: Map[String, Int] = Map()
  private def update(node: Node, script: Script): Unit =
    for (Script(code, _, _, _) <- nodeMap.get(node))
      val count = codeMap(code) - 1
      if (count == 0) codeMap -= code; scriptMap -= code
      codeMap += code -> count
    codeMap += script.code -> (codeMap.get(script.code) match
      case Some(count) => count + 1
      case None        => scriptMap += script.code -> script; 1
    )
    nodeMap += node -> script

  // all meaningful scripts
  def minimalScripts: Vector[Script] = scriptMap.values.toVector

  // the number of all meaningful code set
  def codeCount: Int = codeMap.size

  // all meaningful code set
  def codeSet: Set[String] = codeMap.keySet

  // all meaningful tests
  private var failedTests: Set[(String, ConformTest)] = Set()
  private var transFailedTests: Set[(String, ConformTest)] = Set()
  def doConformTest(initSt: State, finalSt: State) =
    val code = initSt.sourceText.get
    val (test, transTest) = ConformTest.createTestPair(initSt, finalSt)
    if (!test.isPass) failedTests += (code, test)
    if (!transTest.isPass) transFailedTests += (code, transTest)

  // script parser
  private lazy val scriptParser = cfg.scriptParser

  /** evaluate a given ECMAScript program, update coverage, and return
    * evaluation result with whether it succeeds to increase coverage
    */
  def runAndCheck(script: Script): (State, State, Boolean) = {
    val Script(code, ast, name, path) = script

    // program infos
    var markedAst = ast.nodeSet

    // run interpreter and record touched
    var touched: Set[Node] = Set()
    val initSt = Initialize(cfg, code, Some(ast))
    val finalSt = new Interpreter(initSt, timeLimit = timeLimit) {
      // check if current state need to be recorded
      private def needRecord: Boolean =
        val contexts = st.context :: st.callStack.map(_.context)
        val astOpt = contexts.flatMap(_.astOpt).headOption
        astOpt.fold(false)(markedAst contains _)

      // override eval for node
      override def eval(node: Node): Unit =
        // record touched
        if (needRecord) touched += node
        super.eval(node)

      // handle dynamically created ast
      override def eval(expr: Expr): Value =
        val v = super.eval(expr)
        (expr, v) match
          case (_: EParse, AstValue(ast)) if needRecord =>
            markedAst ++= ast.nodeSet
          case _ => /* do nothing */
        v
    }.result

    // update coverage
    var updated = false
    for (node <- touched) nodeMap.get(node) match
      case Some(script) if script.code.length <= code.length =>
      case _ => update(node, script); updated = true

    (initSt, finalSt, updated)
  }

  /** evaluate a given ECMAScript program, update coverage, and return
    * evaluation result
    */
  def run(script: Script): State = { val (_, st, _) = runAndCheck(script); st }

  /** evaluate a given ECMAScript program, update coverage, and return
    * evaluation result
    */
  def run(ast: Ast, filename: String): State =
    run(Script(ast.toString(cfg.grammar), ast, filename, Some(filename)))

  /** get node coverage */
  def nodeCov: (Int, Int) = (nodeMap.size, cfg.nodeMap.size)

  /** get branch coverage */
  // TODO handle return-if-abrupt
  def branchCov: (Int, Int) =
    val branches = cfg.nodeMap.values.collect { case br: Branch => br }
    val count = branches.foldLeft(0) {
      case (acc, branch @ Branch(_, _, _, Some(thenNode), Some(elseNode))) =>
        nodeMap.get(branch) match
          case Some(_) =>
            val t = if (nodeMap contains thenNode) 1 else 0
            val e = if (nodeMap contains elseNode) 1 else 0
            acc + t + e
          case _ => acc
      case (acc, _) => acc
    }
    (count, branches.size * 2)

  /** dump results */
  def dumpTo(
    baseDir: String,
    withScripts: Boolean = false,
    withMsg: Boolean = true,
  ): Unit =
    mkdir(baseDir)
    val covData = for {
      node <- cfg.nodes
    } yield nodeMap
      .get(node)
      .map(script =>
        JsonObject(
          "name" -> script.name.asJson,
          "size" -> script.code.length.asJson,
        ),
      )
    dumpJson(
      name = if (withMsg) Some("coverage") else None,
      data = covData.asJson,
      filename = s"$baseDir/coverage.json",
      noSpace = false,
    )
    rmdir(s"$baseDir/minimal")
    dumpDir(
      name = if (withMsg) Some("Minimal ECMAScript programs") else None,
      iterable = minimalScripts,
      dirname = s"$baseDir/minimal",
      getName = (script: Script) => s"${script.name}.js",
      getData = (script: Script) => script.code,
    )
    rmdir(s"$baseDir/failed")
    type zipped = ((String, ConformTest), Int)
    dumpDir[zipped](
      name = if (withMsg) Some("Failed conformance codes") else None,
      iterable = failedTests.zipWithIndex,
      dirname = s"$baseDir/failed",
      getName = { case ((c, t), i) => s"$i.js" },
      getData = { case ((c, t), i) => c },
    )
    dumpDir[zipped](
      name = if (withMsg) Some("Failed conformance tests") else None,
      iterable = failedTests.zipWithIndex,
      dirname = s"$baseDir/failed",
      getName = { case ((c, t), i) => s"$i.test.js" },
      getData = { case ((c, t), i) => t },
    )
    rmdir(s"$baseDir/trans-failed")
    dumpDir[zipped](
      name = if (withMsg) Some("Failed transpiled conformance codes") else None,
      iterable = transFailedTests.zipWithIndex,
      dirname = s"$baseDir/trans-failed",
      getName = { case ((c, t), i) => s"$i.js" },
      getData = { case ((c, t), i) => c },
    )
    dumpDir[zipped](
      name = if (withMsg) Some("Failed transpiled conformance tests") else None,
      iterable = transFailedTests.zipWithIndex,
      dirname = s"$baseDir/trans-failed",
      getName = { case ((c, t), i) => s"$i.test.js" },
      getData = { case ((c, t), i) => t },
    )

  /** convertion to string */
  private def percent(n: Double, t: Double): Double = n / t * 100
  override def toString: String = {
    val (nCovered, nTotal) = nodeCov
    val (bCovered, bTotal) = branchCov
    val nPercent = percent(nCovered, nTotal)
    val bPercent = percent(bCovered, bTotal)
    f"""- coverage:
       |  - node: $nCovered%,d/$nTotal%,d ($nPercent%2.2f%%)
       |  - branch: $bCovered%,d/$bTotal%,d ($bPercent%2.2f%%)""".stripMargin
  }

  /** extension for AST */
  extension (ast: Ast) {

    /** get all child nodes */
    def nodeSet: Set[Ast] =
      var nodes = Set(ast)
      ast match
        case Syntactic(_, _, _, cs) =>
          for {
            child <- cs.flatten
            childNodes = child.nodeSet
          } nodes ++= childNodes
        case _ => /* do nothing */
      nodes
  }
}
