package esmeta.es.util

import esmeta.LINE_SEP
import esmeta.cfg.*
import esmeta.es.*
import esmeta.es.util.*
import esmeta.interpreter.*
import esmeta.ir.{Expr, EParse, EReturnIfAbrupt, EBool}
import esmeta.state.*
import esmeta.ty.AstSingleTy
import esmeta.util.*
import esmeta.util.SystemUtils.*
import io.circe.*, io.circe.syntax.*
import math.Ordering.Implicits.seqOrdering
import scala.collection.mutable.{Map => MMap, Set => MSet}

/** coverage measurement in CFG */
class Coverage(
  timeLimit: Option[Int] = None,
  synK: Option[Int] = None,
) {

  import Coverage.*

  // all meaningful scripts
  def minimalScripts: Set[Script] = _minimalScripts.toSet

  private val _minimalScripts: MSet[Script] = MSet()

  // target conditional branches
  def targetCondViews: Map[CondView, Option[Nearest]] = _targetCondViews.toMap

  def targetCondSize: Int = _targetCondViews.size

  private val _targetCondViews: MMap[CondView, Option[Nearest]] = MMap()

  // the number of all meaningful code set
  def size: Int = counter.size

  // script reference counter
  private val counter: MMap[Script, Int] = MMap()

  // mapping from nodes to scripts
  def getScript(node: Node): Option[Script] = nodeViewMap.get(NodeView(node))

  def getScript(node: NodeView): Option[Script] = nodeViewMap.get(node)

  def getScript(cond: Cond): Option[Script] = condViewMap.get(CondView(cond))

  def getScript(cond: CondView): Option[Script] = condViewMap.get(cond)

  private val nodes: MSet[Node] = MSet()
  private val nodeViewMap: MMap[NodeView, Script] = MMap()
  private val conds: MSet[Cond] = MSet()
  private val condViewMap: MMap[CondView, Script] = MMap()

  /** evaluate a given ECMAScript program, update coverage, and return
    * evaluation result with whether it succeeds to increase coverage
    */
  def runAndCheck(script: Script): (State, State, Interp, Boolean, Boolean) = {
    val Script(code, name) = script

    // run interpreter and record touched
    val initSt = cfg.init.from(script)
    val interp = Interp(initSt, timeLimit, synK)
    val finalSt = interp.result

    // update node coverage
    var covered = false
    var updated = false
    for ((nodeView, _) <- interp.touchedNodeViews)
      nodeViewMap.get(nodeView) match
        case None =>
          update(nodeView, script); updated = true; covered = true
        case Some(origScript) if origScript.code.length > code.length =>
          update(nodeView, script); updated = true
        case _ =>

    // update branch coverage
    for ((condView, _) <- interp.touchedCondViews)
      condViewMap.get(condView) match
        case None =>
          update(condView, script); updated = true; covered = true
        case Some(origScript) if origScript.code.length > code.length =>
          update(condView, script); updated = true
        case _ =>

    // update target branches
    for ((condView @ CondView(cond, view), loc) <- interp.touchedCondViews)
      val neg = condView.neg
      cond.elem match
        case _ if loc.isEmpty             => /* do nothing */
        case Branch(_, _, EBool(_), _, _) => /* do nothing */
        case ref: WeakUIdRef[EReturnIfAbrupt]
            if !ref.get.check => /* do nothing */
        case _ if condViewMap contains neg => _targetCondViews -= neg
        case _ => _targetCondViews += condView -> loc

    (initSt, finalSt, interp, updated, covered)
  }

  /** evaluate a given ECMAScript program, update coverage, and return
    * evaluation result
    */
  def run(script: Script): State = {
    val (_, st, _, _, _) = runAndCheck(script);
    st
  }

  /** get node coverage */
  def nodeCov: Int = nodes.size

  def nodeViewCov: Int = nodeViewMap.size

  /** get branch coverage */
  def branchCov: Int = conds.size

  def branchViewCov: Int = condViewMap.size

  def dumpToWithDetail(baseDir: String, withMsg: Boolean = true): Unit = dumpTo(
    baseDir = baseDir,
    withScripts = true,
    withtargetCondViews = true,
    withUnreachableFuncs = true,
    withMsg = withMsg,
  )

  /** dump results */
  def dumpTo(
    baseDir: String,
    withScripts: Boolean = false,
    withtargetCondViews: Boolean = false,
    withUnreachableFuncs: Boolean = false,
    withMsg: Boolean = true,
  ): Unit =
    mkdir(baseDir)
    dumpJson(
      name = if (withMsg) Some("node coverage") else None,
      data = nodeViewMapJson,
      filename = s"$baseDir/node-coverage.json",
      space = true,
    )
    dumpJson(
      name = if (withMsg) Some("branch coverage") else None,
      data = condViewMapJson,
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
    if (withtargetCondViews)
      dumpJson(
        name = if (withMsg) Some("target conditional branches") else None,
        data = condViewMapJson(targetCondViews contains _),
        filename = s"$baseDir/target-conds.json",
        space = true,
      )
    if (withUnreachableFuncs)
      val nodes = nodeViewMap.keySet.map(_.node)
      dumpFile(
        name = if (withMsg) Some("unreachable functions") else None,
        data = cfg.funcs
          .filter(f => !nodes.contains(f.entry))
          .map(_.name)
          .sorted
          .mkString(LINE_SEP),
        filename = s"$baseDir/unreach-funcs",
      )

  override def toString: String =
    val app = new Appender
    (app >> "- coverage:")
      .wrap("", "") {
        app :> "- node: " >> nodeCov
        app :> "- branch: " >> branchCov
        synK.map(k =>
          (app :> "- syn-k: " >> k).wrap("", "") {
            app :> "- node: " >> nodeViewCov
            app :> "- branch: " >> branchViewCov
          },
        )
      }
      .toString

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
  private def update(nodeView: NodeView, script: Script): Unit =
    nodes += nodeView.node
    update(nodeView, script, nodeViewMap)

  // update mapping from conditional branches to scripts
  private def update(condView: CondView, script: Script): Unit =
    conds += condView.cond
    update(condView, script, condViewMap)

  // update mapping
  private def update[T](x: T, script: Script, map: MMap[T, Script]): Unit =
    for (script <- map.get(x))
      val count = counter(script) - 1
      if (count == 0) {
        counter -= script; _minimalScripts -= script
      }
      counter += script -> count
    _minimalScripts += script
    counter += script -> (counter.getOrElse(script, 0) + 1)
    map += x -> script

  // script parser
  private lazy val scriptParser = cfg.scriptParser

  // convertion to string
  private def percent(n: Double, t: Double): Double = n / t * 100

  // get JSON for node coverage
  private def nodeViewMapJson: Json = JsonObject(
    (for ((nodeView, script) <- nodeViewMap.toList.sortBy(_._1)) yield {
      val key = nodeView.toString
      val obj = JsonObject(
        "func" -> cfg.funcOf(nodeView.node).name.asJson,
        "loc" -> nodeView.node.loc.map(_.toString).asJson,
        "script" -> script.name.asJson,
      ).asJson
      key -> obj
    }): _*,
  ).asJson

  // get JSON for branch coverage
  private def condViewMapJson: Json = condViewMapJson(_ => true)

  private def condViewMapJson(filter: CondView => Boolean): Json = JsonObject(
    (for {
      (condView, script) <- condViewMap.toList.sortBy(_._1)
      if filter(condView)
    } yield {
      val key = condView.toString
      val obj = JsonObject(
        "func" -> condView.cond.node.map(cfg.funcOf(_).name).asJson,
        "loc" -> condView.cond.loc.map(_.toString).asJson,
        "script" -> script.name.asJson,
      ).asJson
      key -> obj
    }): _*,
  ).asJson
}

object Coverage {

  /** Nearest AST Information */
  type Nearest = (AstSingleTy, Loc)

  /** interpreter */
  class Interp(initSt: State, timeLimit: Option[Int], synK: Option[Int])
    extends Interpreter(
      initSt,
      timeLimit = timeLimit,
      keepProvenance = true,
    ) {
    // program infos
    var touchedNodeViews: Map[NodeView, Option[Nearest]] = Map()
    var touchedCondViews: Map[CondView, Option[Nearest]] = Map()

    // override eval for node
    override def eval(node: Node): Unit =
      // record touched nodes
      touchedNodeViews += NodeView(node, getView) -> getNearest
      super.eval(node)

    // override branch move
    override def moveBranch(branch: Branch, cond: Boolean): Unit =
      // record touched conditional branch
      touchedCondViews += CondView(Cond(branch, cond), getView) -> getNearest
      super.moveBranch(branch, cond)

    // override helper for return-if-abrupt cases
    override def returnIfAbrupt(
      riaExpr: EReturnIfAbrupt,
      value: Value,
      check: Boolean,
    ): Value =
      val abrupt = value.isAbruptCompletion
      touchedCondViews += CondView(
        Cond(riaExpr.idRef, abrupt),
        getView,
      ) -> getNearest
      super.returnIfAbrupt(riaExpr, value, check)

    // get syntax-sensitive views
    private def getView: List[AstSingleTy] =
      synK.fold(Nil)(st.context.sdoList.take(_)).map(_.ty)

    // get location information
    private def getNearest: Option[Nearest] = for {
      sdo <- st.context.sdoList.headOption
      loc <- sdo.ast.loc
    } yield (sdo.ty, loc)
  }

  /* syntax-sensitive views */
  case class NodeView(node: Node, view: List[AstSingleTy] = Nil) {
    override def toString: String = node.simpleString + {
      if (view.isEmpty) ""
      else
        "@" + (view match
          case List(ty) => ty.toString
          case _        => view.mkString("[", ", ", "]")
        )
    }
  }

  case class CondView(cond: Cond, view: List[AstSingleTy] = Nil) {
    def neg: CondView = copy(cond = cond.neg)

    override def toString: String = cond.toString + {
      if (view.isEmpty) ""
      else
        "@" + (view match
          case List(ty) => ty.toString
          case _        => view.mkString("[", ", ", "]")
        )
    }
  }

  /** ordering of syntax-sensitive views */
  given Ordering[AstSingleTy] = Ordering.by(_.toString)

  given Ordering[Node] = Ordering.by(_.id)

  given Ordering[NodeView] = Ordering.by(v => (v.node, v.view))

  given Ordering[Cond] = Ordering.by(cond => (cond.kindString, cond.id))

  given Ordering[CondView] = Ordering.by(v => (v.cond, v.view))

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
}
