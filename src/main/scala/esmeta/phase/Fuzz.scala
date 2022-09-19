package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.es.util.fuzzer.*
import esmeta.util.*
import esmeta.util.SystemUtils.*

/** `fuzz` phase */
case object Fuzz extends Phase[CFG, Set[String]] {
  val name = "fuzz"
  val help = "generate ECMAScript programs for fuzzing."
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Set[String] =
    val scripts: Set[String] = Fuzzer(cfg, config.log, config.timeLimit)

    // sorted by size
    val sorted = scripts.toList.sortBy(_.size).zipWithIndex

    // dump the generated ECMAScript programs
    for (dirname <- config.out)
      dumpDir(
        name = "the generated ECMAScript programs",
        iterable = sorted,
        dirname = dirname,
        getName = { case (_, idx) => s"$idx.js" },
        getData = { case (script, _) => script },
      )

    scripts

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "out",
      StrOption((c, s) => c.out = Some(s)),
      "dump the generated ECMAScript programs to a given directory.",
    ),
    (
      "log",
      BoolOption(c => c.log = true),
      "turn on logging mode.",
    ),
    (
      "timeout",
      NumOption((c, k) => c.timeLimit = Some(k)),
      "set the time limit in seconds (default: no limit).",
    ),
  )
  case class Config(
    var out: Option[String] = None,
    var log: Boolean = false,
    var timeLimit: Option[Int] = None,
  )
}
