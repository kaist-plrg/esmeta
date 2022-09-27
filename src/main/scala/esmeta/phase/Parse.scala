package esmeta.phase

import esmeta.*
import esmeta.es.*
import esmeta.parser.ESParser
import esmeta.spec.Spec
import esmeta.util.*
import esmeta.util.SystemUtils.*

/** `parse` phase */
case object Parse extends Phase[Spec, Ast] {
  val name = "parse"
  val help = "parses an ECMAScript file."
  def apply(
    spec: Spec,
    cmdConfig: CommandConfig,
    config: Config,
  ): Ast =
    val filename = getFirstFilename(cmdConfig, name)
    val ast = ESParser(spec.grammar, config.debug)("Script").fromFile(filename)
    if (config.pprint) println(ast.toString(spec.grammar))
    ast
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "debug",
      BoolOption(c => c.debug = true),
      "turn on debugging mode.",
    ),
    (
      "pprint",
      BoolOption(c => c.pprint = true),
      "print pretty string of parsed source code.",
    ),
  )
  case class Config(
    var debug: Boolean = false,
    var pprint: Boolean = false,
  )
}
