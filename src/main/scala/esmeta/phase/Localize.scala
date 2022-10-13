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
    val nodeViewCoverageJson = ??? // cmdConfig(0)
    val touchedNodeViewJson = ??? // cmdConfig(1)
    val failsMapJson = ??? // cmdConfig(2)

    // parse jsons
    val nodeViewCoverageRaw: Map[NodeView, Info] =
      readJson(nodeViewCoverageJson)
    val nodeView2Function: Map[NodeView, Func] =
      nodeViewCoverageRaw.map { case (v, Info(f, _)) => v -> f }
    val id2NodeView: Map[Int, NodeView] =
      nodeViewCoverageRaw.map { case (v, Info(_, i)) => i -> v }
    val touchedNodeView: Map[Name, Vector[Int]] =
      readJson(touchedNodeViewJson)
    val failsMap: Map[Target, Vector[Name]] =
      readJson(failsMapJson)

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
