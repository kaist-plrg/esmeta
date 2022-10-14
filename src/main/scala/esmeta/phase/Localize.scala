package esmeta.phase

import esmeta.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.util.BaseUtils.*
import io.circe.*

type Func = String
type Target = String
type Name = String
type NodeView = String

/** `localize` phase */
case object Localize extends Phase[Unit, Any] {
  val name = "localize"
  val help = "Localize the bug"

  private var _config: Config = null

  def apply(_unit: Unit, cmdConfig: CommandConfig, config: Config): Unit =
    _config = config
    // name of json files
    val nodeViewCoverageJson = s"$FUZZ_LOG_DIR/recent/node-coverage.json"
    val touchedNodeViewJson = s"$FUZZ_LOG_DIR/recent/minimal-touch-node.json"
    val failsMapJson = s"$CONFORMTEST_LOG_DIR/fails.json"

    // parse jsons
    val nodeViewCoverageRaw: Map[NodeView, Info] =
      readJson(nodeViewCoverageJson)
    val nodeView2Function: Map[NodeView, Func] =
      nodeViewCoverageRaw.map { case (v, Info(f, _)) => v -> f }
    val id2NodeView: Map[Int, NodeView] =
      nodeViewCoverageRaw.map { case (v, Info(_, i)) => i -> v }
    val touchedNodeViewMap: Map[Name, Vector[Int]] =
      readJson(touchedNodeViewJson)
    val failsMap: Map[Target, Set[Name]] =
      readJson(failsMapJson)

    val total = touchedNodeViewMap.keys.toVector
    val nodes = nodeViewCoverageRaw.keys.toVector

    failsMap.foreach((target, fails) => {
      val passNum = total.size - fails.size
      val failNum = fails.size

      // initial stat map
      type StatMap = Map[NodeView, NodeStat]
      val initStatMap = nodes.map(_ -> NodeStat(0, 0, passNum, failNum)).toMap

      // update stat of each node by iterating all tests
      val statMap = total.foldLeft(initStatMap) {
        case (statMap, test) =>
          val isPass = !fails.contains(test)
          val touchedNodeViews = touchedNodeViewMap(test).map(id2NodeView)

          def updateStatMap(statMap: StatMap, nodeView: NodeView) =
            statMap.updatedWith(nodeView)(_.map(_.touch(isPass)))

          touchedNodeViews.foldLeft(statMap)(updateStatMap)
      }

      val sorted = statMap.toSeq.sortBy(_._2.score).reverse
      sorted
        .slice(0, 10)
        .foreach((k, v) => println(s"${nodeView2Function(k)}: ${v.score}"))
      // TODO: function aggregation
    })

  /** used to parse node-coverage json */
  case class Info(func: Func, id: Int = id)
  object Info {
    def id = { _id += 1; _id }
    private var _id = -1
  }
  implicit val decodeInfo: Decoder[Info] = new Decoder[Info] {
    final def apply(c: HCursor): Decoder.Result[Info] =
      for (func <- c.downField("func").as[String])
        yield Info(func)
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
