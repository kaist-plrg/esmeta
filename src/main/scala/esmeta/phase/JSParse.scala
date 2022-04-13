package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.js.*
import esmeta.js.util.{Parser => JSParser, JsonProtocol}
import esmeta.util.SystemUtils.*
import esmeta.util.*

/** `parse` phase */
case object JSParse extends Phase[CFG, Ast] {
  val name = "js-parse"
  val help = "parses a JavaScript file."
  def apply(
    cfg: CFG,
    globalConfig: GlobalConfig,
    config: Config,
  ): Ast = {
    import JsonProtocol.given

    val spec = cfg.program.spec

    // load
    val parsed = config.load match
      case Some(path) => readJson[Ast](path)
      case None =>
        val filename = getFirstFilename(globalConfig, name)
        JSParser(spec.grammar)("Script").fromFile(filename)

    // dump
    for { path <- config.dump }
      dumpJson(parsed, path, noSpace = true)

    parsed
  }

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "load",
      StrOption((c, s) => c.load = Some(s)),
      "load ast from json file.",
    ),
    (
      "dump",
      StrOption((c, s) => c.dump = Some(s)),
      "dump ast to json file.",
    ),
  )
  case class Config(
    var load: Option[String] = None,
    var dump: Option[String] = None,
  )
}
