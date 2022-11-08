package esmeta.phase

import esmeta.*
import esmeta.cfg.*
import esmeta.es.util.*
import esmeta.es.util.Coverage.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.util.BaseUtils.*
import io.circe.*, io.circe.generic.semiauto.*
import io.circe.syntax._
import scala.collection.mutable.{Map => MMap}
import java.io.PrintWriter

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

      val test262Dir = cmdConfig.targets(0)
      val allDir = cmdConfig.targets(1)

      // Compare test 262 with fuzzer
      val header = Vector("sens", "left-only", "both", "right-only")
      val body = for {
        k <- Range(0, 3)
        cp <- List(false, true)
        if (k > 0 || !cp)
      } yield compareCoverage(test262Dir, allDir, k, cp)
      dumpRows(header +: body, s"$HANDLE_COVERAGE_LOG_DIR/test262-cmp.csv")

      // draw #call-path histogram
      drawKGraph(1, allDir)
      drawKGraph(2, allDir)
      drawCpGraph(1, allDir)
      drawCpGraph(2, allDir)
      maximumTxt.flush
    }

  private def compareCoverage(
    dir1: String,
    dir2: String,
    k: Int,
    cp: Boolean,
  ): Vector[String] =
    val proto = jsonProtocol.get
    import proto.given

    val sens = if (cp) then s"$k-cp" else s"$k"

    val nodeJson1 = s"$dir1/node-coverage.json"
    val condJson1 = s"$dir1/branch-coverage.json"
    val nodeJson2 = s"$dir2/$sens/node-coverage.json"
    val condJson2 = s"$dir2/$sens/branch-coverage.json"

    val nodeViewInfos1: List[NodeViewInfo] = readJson(nodeJson1)
    val nodeViews1 = nodeViewInfos1.map(_.nodeView)
    val nodeViewInfos2: List[NodeViewInfo] = readJson(nodeJson2)
    val nodeViews2 = nodeViewInfos2.map(_.nodeView)

    val condViewInfos1: List[CondViewInfo] = readJson(condJson1)
    val condViews1 = condViewInfos1.map(_.condView)
    val condViewInfos2: List[CondViewInfo] = readJson(condJson2)
    val condViews2 = condViewInfos2.map(_.condView)

    val nv1 = nodeViews1.map(_.lower(k, cp)).toSet
    val nv2 = nodeViews2.map(_.lower(k, cp)).toSet
    val nv = nv1 & nv2

    val cv1 = condViews1.map(_.lower(k, cp)).toSet
    val cv2 = condViews2.map(_.lower(k, cp)).toSet
    val cv = cv1 & cv2

    val both = nv.size + cv.size
    val leftOnly = nv1.size + cv1.size - both
    val rightOnly = nv2.size + cv2.size - both

    println(s"[$sens]")
    println(s" - both     : $both")
    println(s" - left only  : $leftOnly")
    println(s" - right only : $rightOnly")

    Vector(sens, leftOnly, both, rightOnly).map(_.toString)

  private def countBy[T, G](iter: Iterable[T], group: T => G): Map[G, Int] =
    iter.groupMapReduce(group)(_ => 1)(_ + _)

  private def drawKGraph(k: Int, basedir: String) =
    println(s"Drawing k-graph for $k...")
    val proto = jsonProtocol.get
    import proto.given

    val dir = s"$basedir/$k"
    val nodeJson = s"$dir/node-coverage.json"
    val condJson = s"$dir/branch-coverage.json"

    val nodeViewInfos: List[NodeViewInfo] = readJson(nodeJson)
    val nodeViews = nodeViewInfos.map(_.nodeView)
    val condViewInfos: List[CondViewInfo] = readJson(condJson)
    val condViews = condViewInfos.map(_.condView)

    val header = Vector("#enclosing", "#test requirement", "#ec * #tr")
    val nodeCount = countBy(nodeViews, _.lower(k - 1, false))
    val condCount = countBy(condViews, _.lower(k - 1, false))
    val totalCount = nodeCount ++ condCount

    println(s"Maximum: ${totalCount.maxBy(_._2)}")
    maximumTxt.println(s"Maximum: ${totalCount.maxBy(_._2)}")

    val cpCount = countBy(totalCount, _._2)
    val body =
      cpCount.toSeq
        .sortBy(_._1)
        .map((k, v) => Vector(k, v, k * v).map(_.toString))

    dumpRows(header +: body, s"$HANDLE_COVERAGE_LOG_DIR/$k-graph.tsv")

  private def drawCpGraph(k: Int, basedir: String) =
    println(s"Drawing cp-graph for $k-cp...")
    val proto = jsonProtocol.get
    import proto.given

    val dir = s"$basedir/$k-cp"
    val nodeJson = s"$dir/node-coverage.json"
    val condJson = s"$dir/branch-coverage.json"

    val nodeViewInfos: List[NodeViewInfo] = readJson(nodeJson)
    val nodeViews = nodeViewInfos.map(_.nodeView)
    val condViewInfos: List[CondViewInfo] = readJson(condJson)
    val condViews = condViewInfos.map(_.condView)

    val header = Vector("#call path", "#test requirement", "#cp * #tr")
    val nodeCount = countBy(nodeViews, _.lower(k, false))
    val condCount = countBy(condViews, _.lower(k, false))
    val totalCount = nodeCount ++ condCount

    println(s"Maximum: ${totalCount.maxBy(_._2)}")
    maximumTxt.println(s"Maximum: ${totalCount.maxBy(_._2)}")

    val cpCount = countBy(totalCount, _._2)
    val body =
      cpCount.toSeq
        .sortBy(_._1)
        .map((k, v) => Vector(k, v, k * v).map(_.toString))

    dumpRows(header +: body, s"$HANDLE_COVERAGE_LOG_DIR/$k-cp-graph.tsv")

  private lazy val maximumTxt: PrintWriter = getPrintWriter(
    s"$HANDLE_COVERAGE_LOG_DIR/maximum.txt",
  )

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List()
  case class Config()
}
