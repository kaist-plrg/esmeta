package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.editor.util.{Parser => ViewParser}
import esmeta.util.SystemUtils.*
import esmeta.editor.sview.SyntacticView

/** `view-parse` phase */
case object ViewParse extends Phase[CFG, (CFG, SyntacticView)] {
  val name = "view-parse"
  val help = "parses a JavaScript file as Syntactic View."
  def apply(
    cfg: CFG,
    globalConfig: GlobalConfig,
    config: Config,
  ): (CFG, SyntacticView) =
    val spec = cfg.program.spec
    val filename = getFirstFilename(globalConfig, name)
    (cfg, ViewParser(spec.grammar)("Script").fromFile(filename))
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List()
  case class Config()
}
