package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.es.util.withCFG
import esmeta.es.util.mutator.*
import esmeta.parser.ESParser
import esmeta.spec.Grammar
import esmeta.util.*
import esmeta.util.SystemUtils.*

/** `mutate` phase */
case object Mutate extends Phase[CFG, String] {
  val name = "mutate"
  val help = "mutates an ECMAScript program."
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): String = withCFG(cfg) {
    val grammar = cfg.grammar
    val filename = getFirstFilename(cmdConfig, this.name)
    val ast = cfg.scriptParser.fromFile(filename)
    val mutator = config.mutator

    // get a mutated AST
    var (_, mutatedAst) = mutator(ast)

    // repeat until the mutated program becomes valid when
    // `-mutate:untilValid` is turned on.
    while (config.untilValid && !mutatedAst.valid(grammar))
      mutatedAst = mutator(ast)._2

    // get string of mutated AST
    val mutated = mutatedAst.toString(grammar)

    // dump the mutated ECMAScript program
    for (filename <- config.out)
      dumpFile(
        name = "the mutated ECMAScript program",
        data = mutated,
        filename = filename,
      )

    mutated
  }

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "out",
      StrOption((c, s) => c.out = Some(s)),
      "dump the mutated ECMAScript program to a given path.",
    ),
    (
      "mutator",
      StrOption((c, s) =>
        c.mutator = s match
          case "random" => RandomMutator()
          case _        => RandomMutator(),
      ),
      "select a mutator (default: random).",
    ),
    (
      "untilValid",
      BoolOption(c => c.untilValid = true),
      "repeat until the mutated program becomes valid.",
    ),
  )
  case class Config(
    var out: Option[String] = None,
    var mutator: Mutator = RandomMutator(),
    var untilValid: Boolean = false,
  )
}
