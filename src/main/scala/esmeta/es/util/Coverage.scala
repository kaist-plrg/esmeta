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

  // script parser
  private lazy val scriptParser = cfg.scriptParser

  /** evaluate a given ECMAScript program, update coverage, and return
    * evaluation result with whether it succeeds to increase coverage
    */
  def runAndCheck(script: Script): (State, Boolean) = {
    val Script(code, ast, name, path) = script

    // program infos
    var markedAst = ast.nodeSet

    // run interpreter and record touched
    var touched: Set[Node] = Set()
    val finalSt = new Interpreter(
      Initialize(cfg, code, Some(ast)),
      timeLimit = timeLimit,
    ) {
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
      case Some(script) if script.code.length <= code.size => /* do nothing */
      case _ =>
        updated = true
        nodeMap += node -> script

    (finalSt, updated)
  }
  def run(ast: Ast, filename: String): State =
    val script =
      Script(ast.toString(cfg.grammar), ast, filename, Some(filename))
    val (st, _) = runAndCheck(script)
    st

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
  def dumpTo(baseDir: String): Unit =
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
    dumpJson(covData.asJson, s"$baseDir/coverage.json")

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
