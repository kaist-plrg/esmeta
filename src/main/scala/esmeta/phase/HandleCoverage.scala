package esmeta.phase

import esmeta.*
import esmeta.cfg.*
import esmeta.es.util.*
import esmeta.es.util.Coverage.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.util.BaseUtils.{error => err, *}
import io.circe.*, io.circe.generic.semiauto.*
import io.circe.syntax._
import scala.collection.mutable.{Map => MMap}

/** `localize` phase */
case object HandleCoverage extends Phase[CFG, Unit] {
  val name = "handle-coverage"
  val help = "HandleCoverage"

  private var _config: Config = null
  private var jsonProtocol: Option[JsonProtocol] = None

  def apply(cfg: CFG, cmdConfig: CommandConfig, config: Config): Unit =
    withCFG(cfg) {
      _config = config
      jsonProtocol = Some(JsonProtocol(cfg))

      if (config.compare.isDefined)
        config.compare.get match
          case "node" =>
            compareNodeCoverage(cmdConfig.targets(0), cmdConfig.targets(1))
          case "branch" =>
            compareBranchCoverage(cmdConfig.targets(0), cmdConfig.targets(1))
          case _ => println("[warn] invalid option")
      else if (config.lower.isDefined)
        config.lower.get match
          case "node"   => lowerNodeCoverage(cmdConfig.targets(0))
          case "branch" => lowerBranchCoverage(cmdConfig.targets(0))
          case _        => println("[warn] invalid option")
      else if (config.cpGraph)
        drawGraph(cmdConfig.targets(0), cmdConfig.targets(1))
      else
        println("[warn] doing nothing")
    }

  private def compareNodeCoverage(json1: String, json2: String) =
    val proto = jsonProtocol.get
    import proto.given

    val nodeViewInfos1: List[NodeViewInfo] = readJson(json1)
    val nodeViewInfos2: List[NodeViewInfo] = readJson(json2)
    val nodeViews1 = nodeViewInfos1.map(_.nodeView)
    val nodeViews2 = nodeViewInfos2.map(_.nodeView)

    for {
      k <- Range(0, _config.kFs + 1)
      cp <- if (_config.cp) then List(false, true) else List(false)
      if (k > 0 || !cp)
      nv1 = nodeViews1.map(_.lower(k, cp)).toSet
      nv2 = nodeViews2.map(_.lower(k, cp)).toSet
      nv = nv1.toSet & nv2.toSet
    } {
      println(s"[$k${if cp then "-cp" else ""}]")
      println(s" - common     : ${nv.size}")
      println(s" - left only  : ${nv1.size - nv.size}")
      println(s" - right only : ${nv2.size - nv.size}")
    }

  private def compareBranchCoverage(json1: String, json2: String) =
    val proto = jsonProtocol.get
    import proto.given

    val condViewInfos1: List[CondViewInfo] = readJson(json1)
    val condViewInfos2: List[CondViewInfo] = readJson(json2)
    val condViews1 = condViewInfos1.map(_.condView)
    val condViews2 = condViewInfos2.map(_.condView)

    for {
      k <- Range(0, _config.kFs + 1)
      cp <- if (_config.cp) then List(false, true) else List(false)
      if (k > 0 || !cp)
      cv1 = condViews1.map(_.lower(k, cp)).toSet
      cv2 = condViews2.map(_.lower(k, cp)).toSet
      cv = cv1.toSet & cv2.toSet
    } {
      println(s"[$k${if cp then "-cp" else ""}]")
      println(s" - common     : ${cv.size}")
      println(s" - left only  : ${cv1.size - cv.size}")
      println(s" - right only : ${cv2.size - cv.size}")
    }

  private def lowerNodeCoverage(json: String) =
    val proto = jsonProtocol.get
    import proto.given

    val nodeViewInfos: List[NodeViewInfo] = readJson(json)
    val nodeViews = nodeViewInfos.map(_.nodeView)

    for {
      k <- Range(0, _config.kFs + 1)
      cp <- if (_config.cp) then List(false, true) else List(false)
      if (k > 0 || !cp)
      nv = nodeViews.map(_.lower(k, cp)).toSet
    } println(s"$k${if cp then "-cp" else "   "} : ${nv.size}")

  private def lowerBranchCoverage(json: String) =
    val proto = jsonProtocol.get
    import proto.given

    val condViewInfos: List[CondViewInfo] = readJson(json)
    val condViews = condViewInfos.map(_.condView)
    for {
      k <- Range(0, _config.kFs + 1)
      cp <- if (_config.cp) then List(false, true) else List(false)
      if (k > 0 || !cp)
      cv = condViews.map(_.lower(k, cp)).distinct
    } println(s"k${if cp then "-cp" else ""} : ${cv.size}")

  private def countBy[T, G](iter: Iterable[T], group: T => G): Map[G, Int] =
    iter.groupMapReduce(group)(_ => 1)(_ + _)

  private def drawGraph(nodeJson: String, condJson: String) =
    val proto = jsonProtocol.get
    import proto.given

    val nodeViewInfos: List[NodeViewInfo] = readJson(nodeJson)
    val nodeViews = nodeViewInfos.map(_.nodeView)
    val condViewInfos: List[CondViewInfo] = readJson(condJson)
    val condViews = condViewInfos.map(_.condView)

    val k = _config.kFs
    if (k == 0)
      err("k should be greater than 0")

    val header = Vector("#call path", "#test requirement", "#cp * #tr")
    val nodeCount = countBy(nodeViews, _.lower(k, false))
    val condCount = countBy(condViews, _.lower(k, false))
    val cpCount = countBy(nodeCount ++ condCount, _._2)
    val body =
      cpCount.toSeq
        .sortBy(_._1)
        .map((k, v) => Vector(k, v, k * v).map(_.toString))

    body.foreach(kv => println(s"${kv(0)}: ${kv(1)}"))
    dumpRows(header +: body, s"$HANDLE_COVERAGE_LOG_DIR/$k-cp-graph.tsv")

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "compare",
      StrOption((c, s) => c.compare = Some(s)),
      "compare two coverages",
    ),
    (
      "lower",
      StrOption((c, s) => c.lower = Some(s)),
      "lower the coverage of the given coverage",
    ),
    (
      "cp-graph",
      BoolOption(c => c.cpGraph = true),
      "extract data for drawing cp",
    ),
    (
      "k-fs",
      NumOption((c, k) => c.kFs = k),
      "maximum sensitivity",
    ),
    (
      "cp",
      BoolOption(c => c.cp = true),
      "use cp",
    ),
  )
  case class Config(
    var compare: Option[String] = None,
    var lower: Option[String] = None,
    var cpGraph: Boolean = false,
    var kFs: Int = 0,
    var cp: Boolean = false,
  )
}
