package esmeta.es.util

import esmeta.cfg.*
import esmeta.es.*
import esmeta.interpreter.*
import esmeta.ir.{Expr, EParse, EReturnIfAbrupt}
import esmeta.state.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import io.circe.*, io.circe.syntax.*
import scala.collection.mutable.{Map => MMap, Set => MSet}

/** coverage measurement in CFG */
class Coverage(
  cfg: CFG,
  timeLimit: Option[Int] = None,
) {

  // all meaningful scripts
  def minimalScripts: Set[Script] = _minimalScripts.toSet
  private val _minimalScripts: MSet[Script] = MSet()

  // target conditional branches
  def targetConds: Set[Cond] = _targetConds.toSet
  private val _targetConds: MSet[Cond] = MSet()

  // the number of all meaningful code set
  def size: Int = counter.size
  // script reference counter
  private val counter: MMap[Script, Int] = MMap()

  // mapping from nodes to scripts
  def getScript(node: Node): Option[Script] = nodeMap.get(node)
  private val nodeMap: MMap[Node, Script] = MMap()

  // mapping from branches to scripts
  def getScript(cond: Cond): Option[Script] = condMap.get(cond)
  private val condMap: MMap[Cond, Script] = MMap()

  // branch or reference to EReturnIfAbrupt with boolean values
  // `true` (`false`) denotes then- (else-) branch or abrupt (non-abrupt) value
  case class Cond(elem: Branch | WeakUIdRef[EReturnIfAbrupt], cond: Boolean) {
    def neg: Cond = copy(cond = !cond)
  }

  /** evaluate a given ECMAScript program, update coverage, and return
    * evaluation result with whether it succeeds to increase coverage
    */
  def runAndCheck(script: Script): (State, State, Boolean) = {
    val Script(code, ast, name, path) = script

    // program infos
    var markedAst = ast.nodeSet

    // run interpreter and record touched
    var touchedNodes: Set[Node] = Set()
    var touchedConds: Set[Cond] = Set()
    val initSt = Initialize(cfg, code, Some(ast))
    val finalSt = new Interpreter(initSt, timeLimit = timeLimit) {
      // check if current state need to be recorded
      private def needRecord: Boolean =
        val contexts = st.context :: st.callStack.map(_.context)
        val astOpt = contexts.flatMap(_.astOpt).headOption
        astOpt.fold(false)(markedAst contains _)

      // override eval for node
      override def eval(node: Node): Unit =
        // record touched nodes
        if (needRecord) touchedNodes += node
        super.eval(node)

      // override branch move
      override def moveBranch(branch: Branch, cond: Boolean): Unit =
        // record touched conditional branch
        if (needRecord) touchedConds += Cond(branch, cond)
        super.moveBranch(branch, cond)

      // override helper for return-if-abrupt cases
      override def returnIfAbrupt(
        riaExpr: EReturnIfAbrupt,
        value: Value,
        check: Boolean,
      ): Value =
        val abrupt = value.isAbruptCompletion
        if (needRecord) touchedConds += Cond(riaExpr.idRef, abrupt)
        super.returnIfAbrupt(riaExpr, value, check)

      // handle dynamically created ast
      override def eval(expr: Expr): Value =
        val v = super.eval(expr)
        (expr, v) match
          case (_: EParse, AstValue(ast)) if needRecord =>
            markedAst ++= ast.nodeSet
          case _ => /* do nothing */
        v
    }.result

    // update node coverage
    var updated = false
    for (node <- touchedNodes) nodeMap.get(node) match
      case Some(script) if script.code.length <= code.length =>
      case _ => update(node, script); updated = true

    // update target branches
    for (cond <- touchedConds) condMap.get(cond) match
      case Some(script) if script.code.length <= code.length =>
      case _ => update(cond, script); updated = true

    // update branch coverage
    for (cond <- touchedConds)
      if (condMap contains cond.neg) _targetConds -= cond.neg
      else _targetConds += cond

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
  def nodeCov: (Int, Int) = (nodeMap.size, nodes.size)
  lazy val nodes: List[Node] = cfg.nodes

  /** get branch coverage */
  def branchCov: (Int, Int) = (condMap.size, conds.size)
  lazy val branches: List[Branch] = cfg.branches
  lazy val riaExprs: List[WeakUIdRef[EReturnIfAbrupt]] =
    cfg.riaExprs.map(_.idRef)
  lazy val conds: List[Cond] = for {
    elem <- (branches ++ riaExprs: List[Branch | WeakUIdRef[EReturnIfAbrupt]])
    bool <- List(true, false)
  } yield Cond(elem, bool)

  /** dump results */
  def dumpTo(
    baseDir: String,
    withScripts: Boolean = false,
    withMsg: Boolean = true,
  ): Unit =
    mkdir(baseDir)
    val covData = for {
      node <- nodes
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
      name = if (withMsg) Some("minimal ECMAScript programs") else None,
      iterable = _minimalScripts,
      dirname = s"$baseDir/minimal",
      getName = (script: Script) => s"${script.name}.js",
      getData = (script: Script) => script.code,
    )

  override def toString: String =
    val (nCovered, nTotal) = nodeCov
    val (bCovered, bTotal) = branchCov
    val nPercent = percent(nCovered, nTotal)
    val bPercent = percent(bCovered, bTotal)
    f"""- coverage:
       |  - node: $nCovered%,d/$nTotal%,d ($nPercent%2.2f%%)
       |  - branch: $bCovered%,d/$bTotal%,d ($bPercent%2.2f%%)""".stripMargin

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  // update mapping from nodes to scripts
  private def update(node: Node, script: Script): Unit =
    update(node, script, nodeMap)
  // update mapping from conditional branches to scripts
  private def update(cond: Cond, script: Script): Unit =
    update(cond, script, condMap)
  // update mapping
  private def update[T](x: T, script: Script, map: MMap[T, Script]): Unit =
    for (script <- map.get(x))
      val count = counter(script) - 1
      if (count == 0) { counter -= script; _minimalScripts -= script }
      counter += script -> count
    _minimalScripts += script
    counter += script -> (counter.getOrElse(script, 0) + 1)
    map += x -> script

  // script parser
  private lazy val scriptParser = cfg.scriptParser

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

  // convertion to string
  private def percent(n: Double, t: Double): Double = n / t * 100
}
