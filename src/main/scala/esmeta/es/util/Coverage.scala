package esmeta.es.util

import esmeta.LINE_SEP
import esmeta.cfg.*
import esmeta.es.*
import esmeta.es.util.*
import esmeta.es.util.injector.*
import esmeta.interpreter.*
import esmeta.ir.{Expr, EParse, EReturnIfAbrupt, EBool}
import esmeta.js.*
import esmeta.state.*
import esmeta.ty.AstSingleTy
import esmeta.util.*
import esmeta.util.SystemUtils.*
import io.circe.*, io.circe.syntax.*
import math.Ordering.Implicits.seqOrdering

/** coverage measurement in CFG */
class Coverage(
  timeLimit: Option[Int] = None,
  kFs: Int = 0,
  cp: Boolean = false,
) {
  import Coverage.{*, given}
  val jsonProtocol = JsonProtocol(cfg)
  import jsonProtocol.given

  // minimal scripts
  def minimalScripts: Set[Script] = _minimalScripts
  private var _minimalScripts: Set[Script] = Set()

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

  // target conditional branches
  def targetCondViews: Map[Cond, Map[View, Option[Nearest]]] = _targetCondViews
  private var _targetCondViews: Map[Cond, Map[View, Option[Nearest]]] = Map()

  /** evaluate a given ECMAScript program, update coverage, and return
    * evaluation result with whether it succeeds to increase coverage
    */
  def runAndCheck(script: Script): (State, Boolean, Boolean) = {
    val interp = run(script.code)
    check(script, interp)
  }

  /** Manually filter out some programs (i.e. awiat as identifier) */
  private def manualFilter(ast: Ast): Unit = {
    val counter = new esmeta.es.util.mutator.Util.AstCounter({
      case Syntactic("IdentifierReference", _, 2, _) => true
      case Syntactic("LabelIdentifier", _, 2, _)     => true
      case Syntactic("BindingIdentifier", _, 2, _)   => true
      case _                                         => false
    })
    if (counter(ast) > 0)
      throw esmeta.error.NotSupported("await as identifier")
  }

  /** evaluate a given ECMAScript program. */
  def run(code: String): Interp = {
    // run interpreter and record touched
    val initSt = cfg.init.from(code)

    // Manually filter out some programs (i.e. awiat as identifier)
    manualFilter(initSt.cachedAst.get)

    val interp = Interp(initSt, timeLimit, kFs, cp)
    interp.result
    interp
  }

  /** update coverage, and return evaluation result with whether it succeeds to
    * increase coverage
    */
  def check(script: Script, interp: Interp): (State, Boolean, Boolean) =
    val Script(code, name) = script
    val finalSt = interp.result

    // covered new elements
    var covered = false
    // updated elements
    var updated = false

    // update node coverage
    for ((nodeView, _) <- interp.touchedNodeViews)
      getScript(nodeView) match
        case None =>
          update(nodeView, script); updated = true; covered = true
        case Some(origScript) if origScript.code.length > code.length =>
          update(nodeView, script); updated = true
        case _ =>

    // update branch coverage
    for ((condView, nearest) <- interp.touchedCondViews)
      getScript(condView) match
        case None =>
          update(condView, nearest, script); updated = true; covered = true
        case Some(origScript) if origScript.code.length > code.length =>
          update(condView, nearest, script); updated = true
        case _ =>

    (finalSt, updated, covered)

  /** get node coverage */
  def nodeCov: Int = nodeViewMap.size
  def nodeViewCov: Int = nodeViews.size

  /** get branch coverage */
  def branchCov: Int = condViewMap.size
  def branchViewCov: Int = condViews.size

  def dumpToWithDetail(baseDir: String, withMsg: Boolean = true): Unit = dumpTo(
    baseDir = baseDir,
    withScripts = true,
    withTargetCondViews = true,
    withUnreachableFuncs = true,
    withMsg = withMsg,
  )

  /** dump results */
  def dumpTo(
    baseDir: String,
    withScripts: Boolean = false,
    withTargetCondViews: Boolean = false,
    withUnreachableFuncs: Boolean = false,
    withMsg: Boolean = true,
  ): Unit =
    mkdir(baseDir)
    lazy val orderedNodeViews = nodeViews.toList.sorted
    lazy val orderedCondViews = condViews.toList.sorted
    lazy val getNodeViewsId = orderedNodeViews.zipWithIndex.toMap
    lazy val getCondViewsId = orderedCondViews.zipWithIndex.toMap
    dumpJson(
      CoverageConstructor(timeLimit, kFs, cp),
      s"$baseDir/constructor.json",
    )

    val st = System.nanoTime()
    def elapsedSec = (System.nanoTime() - st) / 1000000 / 1e3
    def log(msg: Any) =
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
    if (withTargetCondViews)
      dumpJson(
        name = if (withMsg) Some("target conditional branches") else None,
        data = (for {
          (cond, viewMap) <- _targetCondViews
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
      }
    }
    // increse counter of new script
    _minimalScripts += script
    counter += script -> (counter.getOrElse(script, 0) + 1)
    map + (view -> script)

  // remove a cond from targetConds
  private def removeTargetCond(cv: CondView) =
    val CondView(cond, view) = cv
    for (views <- _targetCondViews.get(cond)) {
      val newViews = views - view
      if (newViews.isEmpty)
        _targetCondViews -= cond
      else
        _targetCondViews += cond -> (views - view)
    }

  // add a cond to targetConds
  private def addTargetCond(cv: CondView, nearest: Option[Nearest]) =
    val CondView(cond, view) = cv
    val origViews = _targetCondViews.getOrElse(cond, Map())
    val newViews = origViews + (view -> nearest)
    _targetCondViews += cond -> newViews

  // script parser
  private lazy val scriptParser = cfg.scriptParser

  // conversion to string
  private def percent(n: Double, t: Double): Double = n / t * 100

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
    kFs: Int,
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
      touchedNodeViews += NodeView(node, getView) -> getNearest
      super.eval(node)

    // override branch move
    override def moveBranch(branch: Branch, b: Boolean): Unit =
      // record touched conditional branch
      val cond = Cond(branch, b)
      touchedCondViews += CondView(cond, getView) -> getNearest
      super.moveBranch(branch, b)

    // override helper for return-if-abrupt cases
    override def returnIfAbrupt(
      riaExpr: EReturnIfAbrupt,
      value: Value,
      check: Boolean,
    ): Value =
      val abrupt = value.isAbruptCompletion
      val cond = Cond(riaExpr.idRef, abrupt)
      touchedCondViews += CondView(cond, getView) -> getNearest
      super.returnIfAbrupt(riaExpr, value, check)

    // get syntax-sensitive views
    private def getView: View =
      val stack = st.context.featureStack.take(kFs)
      val path = if (cp) then Some(st.context.callPath) else None
      stack match {
        case Nil                  => None
        case feature :: enclosing => Some(enclosing, feature, path)
      }

    // get location information
    private def getNearest: Option[Nearest] = st.context.nearest
  }

  /** meta-info for each script */
  case class ScriptInfo(
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
  def lowerView(view: View, kFs: Int, cp: Boolean): View =
    view.flatMap {
      case (stack, feature, callPath) =>
        kFs match {
          case 0 => None
          case k =>
            Some((stack.take(k - 1), feature, if cp then callPath else None))
        }
    }
  case class NodeView(node: Node, view: View = None) {
    override def toString: String =
      node.simpleString + stringOfView(view)
    def toFuncView = FuncView(cfg.funcOf(node), view)
    def lower(kFs: Int, cp: Boolean) = NodeView(node, lowerView(view, kFs, cp))
  }
  case class CondView(cond: Cond, view: View = None) {
    def neg: CondView = copy(cond = cond.neg)

    override def toString: String =
      cond.toString + stringOfView(view)

    def lower(kFs: Int, cp: Boolean) = CondView(cond, lowerView(view, kFs, cp))
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

  // meta-info for each view or features
  case class NodeViewInfo(index: Int, nodeView: NodeView, script: String)
  case class CondViewInfo(index: Int, condView: CondView, script: String)

  case class CoverageConstructor(
    timeLimit: Option[Int],
    kFs: Int,
    cp: Boolean,
  )
  def fromLog(baseDir: String): Coverage =
    val jsonProtocol = JsonProtocol(cfg)
    import jsonProtocol.given

    def rj[T](json: String)(implicit decoder: Decoder[T]) =
      readJson[T](s"$baseDir/$json")

    val con: CoverageConstructor = rj(s"constructor.json")
    val cov = new Coverage(con.timeLimit, con.kFs, con.cp)

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
      code = readFile(minimal.getPath).drop(USE_STRICT.length).strip
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
