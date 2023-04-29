package esmeta.es.util

import esmeta.LINE_SEP
import esmeta.cfg.*
import esmeta.es.*
import esmeta.es.util.*
import esmeta.es.util.Coverage.{Cond, CondView, NodeView}
import esmeta.es.util.fuzzer.FSTrie
import esmeta.es.util.injector.*
import esmeta.interpreter.*
import esmeta.ir.{EBool, EParse, EReturnIfAbrupt, Expr}
import esmeta.spec.{BuiltinHead, SyntaxDirectedOperationHead}
import esmeta.state.*
import esmeta.ty.AstSingleTy
import esmeta.util.*
import esmeta.util.SystemUtils.*
import io.circe.*
import io.circe.syntax.*

import scala.collection.immutable.Map
import scala.collection.mutable.Map as MMap
import scala.math.Ordering.Implicits.seqOrdering

/** coverage measurement in CFG */
class Coverage(
  timeLimit: Option[Int] = None,
  kFs: Int = 0,
  cp: Boolean = false,
  onlineNumStdDev: Option[Int] = None,
  fsTrieIn: Option[FSTrie] = None,
) {

  import Coverage.{*, given}

  val jsonProtocol: JsonProtocol = JsonProtocol(cfg)
  import jsonProtocol.given

  // minimal scripts
  def minimalScripts: Set[Script] = _minimalScripts

  private var _minimalScripts: Set[Script] = Set()

  // meta-info of each script
  private var _minimalInfo: Map[String, ScriptInfo] = Map()

  // the number of minimal scripts
  def size: Int = counter.size

  // script reference counter
  private var counter: Map[Script, Int] = Map()

  // get mapping from views to scripts for nodes or conditions
  def apply(node: Node): Map[View, Script] = nodeViewMap.getOrElse(node, Map())

  def apply(cond: Cond): Map[View, Script] = condViewMap.getOrElse(cond, Map())

  // get script from nodes or conditions
  def getScript(nv: NodeView): Option[Script] = apply(nv.node).get(nv.view)

  def getScript(cv: CondView): Option[Script] = apply(cv.cond).get(cv.view)

  // mapping from nodes/conditions to scripts
  private var nodeViewMap: Map[Node, Map[View, Script]] = Map()
  private var nodeViews: Set[NodeView] = Set()
  private var condViewMap: Map[Cond, Map[View, Script]] = Map()
  private var condViews: Set[CondView] = Set()

  // mapping from feature stacks to number of touches
  private var fsTrie: FSTrie = fsTrieIn.getOrElse(FSTrie.root)

  private var scriptIter: Int = 0
  private val fsTrieUpdateIter: Int = 1024

  def updateSensitivity(): Unit =
    if (fsTrieIn.isEmpty)
      fsTrie = fsTrie.splitMax(onlineNumStdDev)

  // target conditional branches
  def targetCondViews: Map[Cond, Map[View, Option[Nearest]]] = _targetCondViews

  private var _targetCondViews: Map[Cond, Map[View, Option[Nearest]]] = Map()

  /** evaluate a given ECMAScript program, update coverage, and return
    * evaluation result with whether it succeeds to increase coverage
    */
  def runAndCheck(
    script: Script,
    isBugOpt: Option[Boolean] = None,
  ): (State, Boolean, Boolean) = {
    val interp = run(script.code)
    check(script, interp, isBugOpt)
  }

  /** evaluate a given ECMAScript program. */
  def run(code: String): Interp = {
    // run interpreter and record touched
    val initSt = cfg.init.from(code)
    val interp = Interp(
      initSt,
      timeLimit,
      cp,
    )
    interp.result
    interp
  }

  def check(
    script: Script,
    interp: Interp,
    isBugOpt: Option[Boolean] = None,
  ): (State, Boolean, Boolean) =
    val (finalSt, updated, covered, _) =
      checkWithBlocking(script, interp, isBugOpt)
    (finalSt, updated, covered)

  /** update coverage, and return evaluation result with whether it succeeds to
    * increase coverage
    */
  private def checkWithBlocking(
    script: Script,
    interp: Interp,
    isBugOpt: Option[Boolean] = None,
  ): (State, Boolean, Boolean, Set[Script]) =
    val Script(code, _) = script
    val initSt =
      cfg.init.from(code) // TODO: Check if recreating init state is OK
    val finalSt = interp.result

    // covered new elements
    var covered = false
    // updated elements
    var updated = false
    // Script that block the update
    var blockingScripts: Set[Script] = Set.empty

    var touchedNodeViews: Map[NodeView, Option[Nearest]] = Map()
    var touchedCondViews: Map[CondView, Option[Nearest]] = Map()

    // increment touch of raw feature stacks
    if (onlineNumStdDev.isDefined) {
      val touchedRawNodeStack = interp.touchedNodeViews.keys
        .flatMap(_.view)
        .map(v => v._2 :: v._1)
        .map(_.map(_.func.name))
        .toSet

      val touchedRawCondStack = interp.touchedCondViews.keys
        .flatMap(_.view)
        .map(v => v._2 :: v._1)
        .map(_.map(_.func.name))
        .toSet

      touchedRawNodeStack.foreach { rawStack =>
        fsTrie = fsTrie.incTouch(rawStack, isBugOpt)
      }
      touchedRawCondStack.foreach { rawStack =>
        fsTrie = fsTrie.incTouch(rawStack, isBugOpt)
      }
    }

    // update node coverage
    for ((rawNodeView, temp) <- interp.touchedNodeViews)
      // cook NodeView
      val NodeView(node, rawView) = rawNodeView
      val view: View = rawView match {
        case None => None
        case Some((rawEnclosing, feature, path)) =>
          val featureStack = cookFeatureStack(feature :: rawEnclosing)
          if featureStack.isEmpty then None
          else Some((featureStack.tail, featureStack.head, path))
      }
      val nodeView = NodeView(node, view)
      touchedNodeViews += nodeView -> temp

      getScript(nodeView) match
        case None =>
          update(nodeView, script); updated = true; covered = true
        case Some(origScript) if origScript.code.length > code.length =>
          update(nodeView, script)
          updated = true
          blockingScripts += origScript
        case Some(blockScript) => blockingScripts += blockScript

    // update branch coverage
    for ((rawCondView, nearest) <- interp.touchedCondViews)
      // cook condView
      val CondView(cond, rawView) = rawCondView
      val view: View = rawView match {
        case None => None
        case Some((rawEnclosing, feature, path)) =>
          val featureStack = cookFeatureStack(feature :: rawEnclosing)
          if featureStack.isEmpty then None
          else Some((featureStack.tail, featureStack.head, path))
      }
      val condView = CondView(cond, view)
      touchedCondViews += condView -> nearest

      getScript(condView) match
        case None =>
          update(condView, nearest, script); updated = true; covered = true
        case Some(origScript) if origScript.code.length > code.length =>
          update(condView, nearest, script)
          updated = true
          blockingScripts += origScript
        case Some(blockScript) => blockingScripts += blockScript

    // update script info
    if (updated)
      _minimalInfo += script.name -> ScriptInfo(
        ConformTest.createTest(finalSt),
        touchedNodeViews.keys,
        touchedCondViews.keys,
      )
    // assert: _minimalScripts ~= _minimalInfo.keys

    // update FSTrie
    scriptIter += 1
    if (scriptIter >= fsTrieUpdateIter) {
      scriptIter = 0
      updateSensitivity()
    }

    (finalSt, updated, covered, blockingScripts)

  private def cookFeatureStack(
    rawFeatureStack: List[Feature],
  ): List[Feature] = {
    // online selection
    if onlineNumStdDev.isDefined then {
      //      println(s"length: ${rawFeatureStack.size}")
      rawFeatureStack.take({
        val temp = fsTrie.getViewLength(rawFeatureStack.map(_.func.name))
        //        println(s"temp: $temp")
        temp
      })
    } else {
      // no selection
      rawFeatureStack.take(kFs)
    }
  }

  /** get node coverage */
  def nodeCov: Int = nodeViewMap.size

  def nodeViewCov: Int = nodeViews.size

  /** get branch coverage */
  def branchCov: Int = condViewMap.size

  def branchViewCov: Int = condViews.size

  def dumpToWithDetail(baseDir: String, withMsg: Boolean = true): Unit = dumpTo(
    baseDir = baseDir,
    withScripts = true,
    withScriptInfo = true,
    withTargetCondViews = true,
    withUnreachableFuncs = true,
    withKMaps = true,
    withPValues = true,
    withFSTrie = true,
    withMsg = withMsg,
  )

  /** dump results */
  def dumpTo(
    baseDir: String,
    withScripts: Boolean = false,
    withScriptInfo: Boolean = false,
    withTargetCondViews: Boolean = false,
    withUnreachableFuncs: Boolean = false,
    withKMaps: Boolean = false,
    withPValues: Boolean = false,
    withFSTrie: Boolean = false,
    withMsg: Boolean = true,
  ): Unit =
    mkdir(baseDir)
    lazy val orderedNodeViews = nodeViews.toList.sorted
    lazy val orderedCondViews = condViews.toList.sorted
    lazy val getNodeViewsId = orderedNodeViews.zipWithIndex.toMap
    lazy val getCondViewsId = orderedCondViews.zipWithIndex.toMap
    dumpJson(
      CoverageConstructor(timeLimit, kFs, cp, onlineNumStdDev),
      s"$baseDir/constructor.json",
    )

    val st = System.nanoTime()

    def elapsedSec = (System.nanoTime() - st) / 1000000 / 1e3

    def log(msg: Any): Unit =
      if (withMsg) println(s"[$elapsedSec s] $msg")

    dumpJson(
      name = if (withMsg) Some("node coverage") else None,
      data = nodeViewInfos(orderedNodeViews),
      filename = s"$baseDir/node-coverage.json",
      space = true,
    )
    log("Dupmed node coverage")
    dumpJson(
      name = if (withMsg) Some("branch coverage") else None,
      data = condViewInfos(orderedCondViews),
      filename = s"$baseDir/branch-coverage.json",
      space = true,
    )
    log("Dupmed branch coverage")
    if (withScripts)
      dumpDir[Script](
        name = if (withMsg) Some("minimal ECMAScript programs") else None,
        iterable = _minimalScripts,
        dirname = s"$baseDir/minimal",
        getName = _.name,
        getData = USE_STRICT + _.code + LINE_SEP,
        remove = true,
      )
      log("Dupmed scripts")
    if (withScriptInfo) {
      dumpDir[(String, ScriptInfo)](
        name = if (withMsg) Some("minimal ECMAScript assertions") else None,
        iterable = _minimalInfo,
        dirname = s"$baseDir/minimal-assertion",
        getName = _._1,
        getData = _._2.test.core, // TODO: dump this as json?
        remove = true,
      )
      log("Dupmed assertions")
      if (withFSTrie && onlineNumStdDev.isDefined) {
        dumpJson(
          name = None,
          data = fsTrie.trim(), // for evaluation
          filename = s"$baseDir/fs_trie.json",
          space = true,
        )
      }
      dumpJson(
        name =
          if (withMsg) Some("list of touched node view of minimal programs")
          else None,
        data = minimalTouchNodeViewJson(getNodeViewsId),
        filename = s"$baseDir/minimal-touch-nodeview.json",
        space = false,
      )
      log("dumped touched node views")
      dumpJson(
        name =
          if (withMsg) Some("list of touched cond view of minimal programs")
          else None,
        data = minimalTouchCondViewJson(getCondViewsId),
        filename = s"$baseDir/minimal-touch-condview.json",
        space = false,
      )
      log("dumped touched cond views")
    }
    if (withTargetCondViews)
      dumpJson(
        name = if (withMsg) Some("target conditional branches") else None,
        data = (for {
          (cond, viewMap) <- _targetCondViews;
          (view, _) <- viewMap
        } yield getCondViewsId(CondView(cond, view))).toSeq.sorted.asJson,
        filename = s"$baseDir/target-conds.json",
        space = true,
      )
      log("dumped target conds")
    if (withUnreachableFuncs)
      dumpFile(
        name = if (withMsg) Some("unreachable functions") else None,
        data = cfg.funcs
          .filter(f => !nodeViewMap.contains(f.entry))
          .map(_.name)
          .sorted
          .mkString(LINE_SEP),
        filename = s"$baseDir/unreach-funcs",
      )
      log("dumped unreachable functions")

  override def toString: String =
    val app = new Appender
    (app >> "- coverage:").wrap("", "") {
      app :> "- node: " >> nodeCov
      app :> "- branch: " >> branchCov
    }
    if (kFs > 0) (app :> "- sensitive coverage:").wrap("", "") {
      app :> "- node: " >> nodeViewCov
      app :> "- branch: " >> branchViewCov
    }
    app.toString

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
    nodeViews += nodeView
    val NodeView(node, view) = nodeView
    nodeViewMap += node -> updated(apply(node), view, script)

  // update mapping from conditional branches to scripts
  private def update(
    condView: CondView,
    nearest: Option[Nearest],
    script: Script,
  ): Unit =
    condViews += condView
    val CondView(cond, view) = condView

    // update target branches
    val neg = condView.neg
    cond.elem match
      case _ if nearest.isEmpty                    => /* do nothing */
      case Branch(_, _, EBool(_), _, _)            => /* do nothing */
      case b: Branch if b.isChildPresentCheck(cfg) => /* do nothing */
      case ref: WeakUIdRef[EReturnIfAbrupt]
          if !ref.get.check => /* do nothing */
      case _ if getScript(neg).isDefined => removeTargetCond(neg)
      case _                             => addTargetCond(condView, nearest)

    condViewMap += cond -> updated(apply(cond), view, script)

  // update mapping
  private def updated[View](
    map: Map[View, Script],
    view: View,
    script: Script,
  ): Map[View, Script] =
    // decrease counter of original script
    for (origScript <- map.get(view)) {
      val count = counter(origScript) - 1
      counter += (origScript -> count)
      if (count == 0) {
        counter -= origScript
        _minimalScripts -= origScript
        _minimalInfo -= origScript.name
      }
    }
    // increse counter of new script
    _minimalScripts += script
    counter += script -> (counter.getOrElse(script, 0) + 1)
    map + (view -> script)

  // remove a cond from targetConds
  private def removeTargetCond(cv: CondView): Unit =
    val CondView(cond, view) = cv
    for (views <- _targetCondViews.get(cond)) {
      val newViews = views - view
      if (newViews.isEmpty)
        _targetCondViews -= cond
      else
        _targetCondViews += cond -> (views - view)
    }

  // add a cond to targetConds
  private def addTargetCond(cv: CondView, nearest: Option[Nearest]): Unit =
    val CondView(cond, view) = cv
    val origViews = _targetCondViews.getOrElse(cond, Map())
    val newViews = origViews + (view -> nearest)
    _targetCondViews += cond -> newViews

  // script parser
  private lazy val scriptParser = cfg.scriptParser

  // conversion to string
  private def percent(n: Double, t: Double): Double = n / t * 100

  // get JSON for touched node view of minimal
  private def minimalTouchNodeViewJson(getId: Map[NodeView, Int]): Json =
    Json.fromFields(
      _minimalInfo.map((name, info) =>
        name -> info.touchedNodeViews.map(getId).asJson,
      ),
    )

  // get JSON for touched cond view of minimal
  private def minimalTouchCondViewJson(getId: Map[CondView, Int]): Json =
    Json.fromFields(
      _minimalInfo.map((name, info) =>
        name -> info.touchedCondViews.map(getId).asJson,
      ),
    )

  // get JSON for node coverage
  private def nodeViewInfos(ordered: List[NodeView]): List[NodeViewInfo] =
    for {
      (nodeView, idx) <- ordered.zipWithIndex
      script <- getScript(nodeView)
    } yield NodeViewInfo(idx, nodeView, script.name)

  // get JSON for branch coverage
  private def condViewInfos(ordered: List[CondView]): List[CondViewInfo] =
    for {
      (condView, idx) <- ordered.zipWithIndex
      script <- getScript(condView)
    } yield CondViewInfo(idx, condView, script.name)
}

object Coverage {

  /** interpreter */
  class Interp(
    initSt: State,
    timeLimit: Option[Int],
    cp: Boolean,
  ) extends Interpreter(
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
      touchedNodeViews += NodeView(node, getView(node)) -> getNearest
      super.eval(node)

    // override branch move
    override def moveBranch(branch: Branch, b: Boolean): Unit =
      // record touched conditional branch
      val cond = Cond(branch, b)
      touchedCondViews += CondView(cond, getView(cond)) -> getNearest
      super.moveBranch(branch, b)

    // override helper for return-if-abrupt cases
    override def returnIfAbrupt(
      riaExpr: EReturnIfAbrupt,
      value: Value,
      check: Boolean,
    ): Value =
      val abrupt = value.isAbruptCompletion
      val cond = Cond(riaExpr.idRef, abrupt)

      touchedCondViews += CondView(cond, getView(cond)) -> getNearest
      super.returnIfAbrupt(riaExpr, value, check)

    // get syntax-sensitive views
    // TODO: NOTE that it is RAW
    private def getView(node: Node | Cond): View =
      val stack = st.context.featureStack
      val path = if cp then Some(st.context.callPath) else None
      stack match {
        case Nil                  => None
        case feature :: enclosing => Some(enclosing, feature, path)
      }

    // get location information
    private def getNearest: Option[Nearest] = st.context.nearest
  }

  /** meta-info for each script */
  private case class ScriptInfo(
    test: ConformTest,
    touchedNodeViews: Iterable[NodeView],
    touchedCondViews: Iterable[CondView],
  )

  /* syntax-sensitive views */
  type View = Option[(List[Feature], Feature, Option[CallPath])]

  private def stringOfView(view: View) = view.fold("") {
    case (enclosing, feature, path) =>
      s"@ $feature${enclosing.mkString("[", ", ", "]")}:${path.getOrElse("")}"
  }

  private def lowerView(view: View, kFs: Int, cp: Boolean): View =
    view.flatMap {
      case (stack, feature, callPath) =>
        kFs match {
          case 0 => None
          case k =>
            Some((stack.take(k - 1), feature, if cp then callPath else None))
        }
    }

  sealed trait NodeOrCondView(view: View) {
    def getView: View = view
  }

  case class NodeView(node: Node, view: View = None)
    extends NodeOrCondView(view) {
    override def toString: String =
      node.simpleString + stringOfView(view)

    def toFuncView: FuncView = FuncView(cfg.funcOf(node), view)

    def lower(kFs: Int, cp: Boolean): NodeView =
      NodeView(node, lowerView(view, kFs, cp))
  }

  case class CondView(cond: Cond, view: View = None)
    extends NodeOrCondView(view) {
    def neg: CondView = copy(cond = cond.neg)

    override def toString: String =
      cond.toString + stringOfView(view)

    def lower(kFs: Int, cp: Boolean): CondView =
      CondView(cond, lowerView(view, kFs, cp))
  }

  case class FuncView(func: Func, view: View = None) {
    override def toString: String =
      func.name + stringOfView(view)
  }

  /** ordering of syntax-sensitive views */
  given Ordering[Feature] = Ordering.by(_.toString)

  given Ordering[CallPath] = Ordering.by(_.toString)

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
      case branch: Branch                   => Some(branch)
      case ref: WeakUIdRef[EReturnIfAbrupt] => ref.get.cfgNode

    // get loc
    def loc: Option[Loc] = elem match
      case branch: Branch                   => branch.loc
      case ref: WeakUIdRef[EReturnIfAbrupt] => ref.get.loc

    // conversion to string
    override def toString: String = s"$kindString[$id]:$condString"

    def simpleString: String = s"$shortKindString[$id]:$condString"
  }

  // meta-info for each view or features
  case class NodeViewInfo(index: Int, nodeView: NodeView, script: String)

  case class CondViewInfo(index: Int, condView: CondView, script: String)

  // TODO: How to show selective-k sensitivity?
  case class CoverageConstructor(
    timeLimit: Option[Int],
    kFs: Int,
    cp: Boolean,
    onlineNumStdDev: Option[Int],
  )

  def fromLog(
    baseDir: String,
  ): Coverage =
    val jsonProtocol = JsonProtocol(cfg)
    import jsonProtocol.given

    def rj[T](json: String)(implicit decoder: Decoder[T]) =
      readJson[T](s"$baseDir/$json")

    val nodeKMap =
      (try {
        rj[List[(String, Int)]]("k-selection/node.json").toMap
      } catch {
        case e: Throwable =>
          print("Coverage.fromLog: ");
          println(e.getMessage);
          Map[String, Int]()
      }).withDefaultValue(0)

    val condKMap =
      (try {
        rj[List[(String, Int)]]("k-selection/cond.json").toMap
      } catch {
        case e: Throwable =>
          print("Coverage.fromLog: ");
          println(e.getMessage);
          Map[String, Int]()
      }).withDefaultValue(0)

    val pValueMap =
      try {
        Some(rj[List[(String, Double)]]("p_values.json").toMap)
      } catch {
        case e: Throwable =>
          print("Coverage.fromLog: ");
          println(e.getMessage);
          None
      }

    var online = true
    val fsTrie =
      try {
        rj[FSTrie]("fs_trie.json")
      } catch {
        case e: Throwable =>
          print("Coverage.fromLog: ");
          println(e.getMessage);
          online = false
          FSTrie.root
      }

    val con: CoverageConstructor = rj(s"constructor.json")
    val cov =
      new Coverage(
        con.timeLimit,
        con.kFs,
        con.cp,
        con.onlineNumStdDev,
        Some(fsTrie),
      )

    val nodeViewInfos: Vector[NodeViewInfo] = rj("node-coverage.json")
    val condViewInfos: Vector[CondViewInfo] = rj("branch-coverage.json")

    val minimalTouchNodeView: Map[String, Vector[Int]] = rj(
      "minimal-touch-nodeview.json",
    )
    val minimalTouchCondView: Map[String, Vector[Int]] = rj(
      "minimal-touch-condview.json",
    )

    for {
      minimal <- listFiles(s"$baseDir/minimal")
      name = minimal.getName
      code = readFile(minimal.getPath).drop(USE_STRICT.length).trim()
      script = Script(code, name)
    } {
      minimalTouchNodeView(name).foreach(i =>
        cov.update(nodeViewInfos(i).nodeView, script),
      )
      minimalTouchCondView(name).foreach(i =>
        cov.update(condViewInfos(i).condView, None, script),
      )
    }

    // TODO: read assertions, and recover complete minimal infos
    // TODO: Recover target conds

    cov
}
