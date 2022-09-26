package esmeta.es.util

import esmeta.LINE_SEP
import esmeta.cfg.*
import esmeta.es.*
import esmeta.interpreter.*
import esmeta.ir.{Expr, EParse, EReturnIfAbrupt, EBool}
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
    // negation
    def neg: Cond = copy(cond = !cond)
    // short kind string
    def kindString: String = elem match
      case (branch: Branch)     => "Branch"
      case (ref: WeakUIdRef[_]) => "EReturnIfAbrupt"
    def shortKindString: String = kindString.take(1)
    // get id
    def id: Int = elem match
      case (branch: Branch)     => branch.id
      case (ref: WeakUIdRef[_]) => ref.id
    // condition string
    def condString: String = if (cond) "T" else "F"
    // get node
    def node: Option[Node] = elem match
      case (branch: Branch)                   => Some(branch)
      case (ref: WeakUIdRef[EReturnIfAbrupt]) => ref.get.cfgNode
    // get loc
    def loc: Option[Loc] = elem match
      case (branch: Branch)                   => branch.loc
      case (ref: WeakUIdRef[EReturnIfAbrupt]) => ref.get.loc
    // conversion to string
    override def toString: String = s"$kindString[$id]:$condString"
    def simpleString: String = s"$shortKindString[$id]:$condString"
  }

  /** ordering of conditional branches */
  given Ordering[Cond] = Ordering.by(cond => (cond.kindString, cond.id))

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
      cond.elem match
        case Branch(_, _, EBool(_), _, _) => /* do nothing */
        case ref: WeakUIdRef[EReturnIfAbrupt]
            if !ref.get.check => /* do nothing */
        case _ if condMap contains cond.neg => _targetConds -= cond.neg
        case _                              => _targetConds += cond

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
  lazy val nodes: List[Node] = cfg.nodes.sortBy(_.id)

  /** get branch coverage */
  def branchCov: (Int, Int) = (condMap.size, conds.size)
  lazy val branches: List[Branch] = cfg.branches
  lazy val riaExprs: List[WeakUIdRef[EReturnIfAbrupt]] =
    cfg.riaExprs.map(_.idRef)
  lazy val conds: List[Cond] = (for {
    elem <- (branches ++ riaExprs: List[Branch | WeakUIdRef[EReturnIfAbrupt]])
    bool <- List(true, false)
  } yield Cond(elem, bool)).sorted

  def dumpToWithDetail(baseDir: String, withMsg: Boolean = true): Unit = dumpTo(
    baseDir = baseDir,
    withScripts = true,
    withTargetConds = true,
    withUnreachableFuncs = true,
    withMsg = withMsg,
  )

  /** dump results */
  def dumpTo(
    baseDir: String,
    withScripts: Boolean = false,
    withTargetConds: Boolean = false,
    withUnreachableFuncs: Boolean = false,
    withMsg: Boolean = true,
  ): Unit =
    mkdir(baseDir)
    dumpJson(
      name = if (withMsg) Some("node coverage") else None,
      data = nodeMapJson,
      filename = s"$baseDir/node-coverage.json",
      space = true,
    )
    dumpJson(
      name = if (withMsg) Some("branch coverage") else None,
      data = condMapJson,
      filename = s"$baseDir/branch-coverage.json",
      space = true,
    )
    if (withScripts)
      dumpDir[Script](
        name = if (withMsg) Some("minimal ECMAScript programs") else None,
        iterable = _minimalScripts,
        dirname = s"$baseDir/minimal",
        getName = _.name,
        getData = _.code,
        remove = true,
      )
    if (withTargetConds)
      dumpJson(
        name = if (withMsg) Some("target conditional branches") else None,
        data = condMapJson(targetConds contains _),
        filename = s"$baseDir/target-conds.json",
        space = true,
      )
    if (withUnreachableFuncs)
      dumpFile(
        name = if (withMsg) Some("unreachable functions") else None,
        data = cfg.funcs
          .filter(f => !nodeMap.contains(f.entry))
          .map(_.name)
          .sorted
          .mkString(LINE_SEP),
        filename = s"$baseDir/unreach-funcs",
      )

  override def toString: String =
    val (nCovered, nTotal) = nodeCov
    val (bCovered, bTotal) = branchCov
    val nPercent = percent(nCovered, nTotal)
    val bPercent = percent(bCovered, bTotal)
    f"""- coverage:
       |  - node: $nCovered%,d/$nTotal%,d ($nPercent%2.2f%%)
       |  - branch: $bCovered%,d/$bTotal%,d ($bPercent%2.2f%%)""".stripMargin

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

  // convertion to string
  private def percent(n: Double, t: Double): Double = n / t * 100

  // get JSON for node coverage
  private def nodeMapJson: Json = JsonObject(
    (for (node <- nodes) yield {
      val key = node.simpleString
      val obj = JsonObject(
        "func" -> cfg.funcOf(node).name.asJson,
        "loc" -> node.loc.map(_.toString).asJson,
        "script" -> nodeMap.get(node).map(_.name).asJson,
      ).asJson
      key -> obj
    }): _*,
  ).asJson

  // get JSON for branch coverage
  private def condMapJson: Json = condMapJson(_ => true)
  private def condMapJson(filter: Cond => Boolean): Json = JsonObject(
    (for (cond <- conds) yield {
      val key = cond.toString
      val obj = JsonObject(
        "func" -> cond.node.map(n => cfg.funcOf(n).name).asJson,
        "loc" -> cond.loc.map(_.toString).asJson,
        "script" -> condMap.get(cond).map(_.name).asJson,
      ).asJson
      key -> obj
    }): _*,
  ).asJson
}
