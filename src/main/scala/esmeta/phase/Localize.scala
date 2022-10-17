package esmeta.phase

import esmeta.*
import esmeta.cfg.*
import esmeta.es.util.*
import esmeta.es.util.Coverage.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.util.BaseUtils.*
import io.circe.*, io.circe.generic.semiauto.*
import scala.collection.mutable.{Map => MMap}

/** `localize` phase */
case object Localize
  extends Phase[CFG, Map[String, Map[String, Seq[(FuncView, Double)]]]] {
  val name = "localize"
  val help = "Localize the bug"

  type Target = String
  type Name = String
  type Result = Map[Target, Map[Name, Seq[(FuncView, Double)]]]
  type MResult = MMap[Target, MMap[Name, Seq[(FuncView, Double)]]]

  private var _config: Config = null

  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Result = withCFG(cfg) {
    val jsonProtocol = JsonProtocol(cfg)
    import jsonProtocol.given

    _config = config
    // name of json files
    val nodeViewCoverageJson = s"$FUZZ_LOG_DIR/recent/node-coverage.json"
    val touchedNodeViewJson = s"$FUZZ_LOG_DIR/recent/minimal-touch-node.json"
    val failsMapJson = s"$CONFORMTEST_LOG_DIR/fails.json"

    // used to parse node-coverage json */
    case class Info(index: Int, nodeView: NodeView)
    given infoDecoder: Decoder[Info] = deriveDecoder

    // parse jsons
    val nodeViewCoverageRaw: Vector[Info] =
      readJson(nodeViewCoverageJson)
    val id2NodeView: Map[Int, NodeView] =
      nodeViewCoverageRaw.map { case Info(i, v) => i -> v }.toMap
    val touchedNodeViewMap: Map[Name, Vector[Int]] =
      readJson(touchedNodeViewJson)
    val failsMap: Map[Target, Set[Name]] =
      readJson(failsMapJson)

    val total = touchedNodeViewMap.keys.toVector
    val nodes = nodeViewCoverageRaw.map(_.nodeView).toVector

    var result: MResult = MMap()

    failsMap.foreach((target, fails) => {
      val passes = total.filterNot(fails.contains(_))
      val passNum = passes.size
      val failNum = fails.size

      // initial stat map
      val initStatMap = nodes.map(_ -> NodeStat(0, 0, passNum, 1)).toMap

      // update stat of each node by iterating all successful tests
      val passStatMap = passes.foldLeft(initStatMap) {
        case (statMap, test) =>
          val touchedNodeViews = touchedNodeViewMap(test).map(id2NodeView)
          touchedNodeViews.foldLeft(statMap)(updateStatMap(true))
      }

      result(target) = MMap()

      // calculate score for each failed tests
      fails.foreach(fail => {
        val touchedNodeViews = touchedNodeViewMap(fail).map(id2NodeView)
        val statMap =
          touchedNodeViews.foldLeft(passStatMap)(updateStatMap(false))

        val aggregated = statMap.toSeq.groupMapReduce {
          case (nodeView, _) => nodeView.toFuncView
        }(_._2.score)(_ max _)

        val sorted = aggregated.toSeq.sortBy(_._2).reverse
        result(target)(fail) = sorted.slice(0, 10).toSeq
      })
    })

    dumpJson(result, s"$LOCALIZE_LOG_DIR/localize.json")

    result.toMap.map((k, v) => k -> v.toMap)
  }

  /** quadruple of node stat */
  case class NodeStat(ep: Int, ef: Int, np: Int, nf: Int) {
    def touch(isPass: Boolean): NodeStat = {
      if (isPass)
        NodeStat(ep + 1, ef, np - 1, nf)
      else
        NodeStat(ep, ef + 1, np, nf - 1)
    }
    lazy val score = ef - ep / (ep + np + 1.0)
  }

  private type StatMap = Map[NodeView, NodeStat]
  private def updateStatMap(pass: Boolean) =
    (statMap: StatMap, nodeView: NodeView) =>
      statMap.updatedWith(nodeView)(_.map(_.touch(pass)))

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "debug",
      BoolOption(c => c.debug = true),
      "turn on debug mode",
    ),
  )
  case class Config(
    var debug: Boolean = false,
  )
}
