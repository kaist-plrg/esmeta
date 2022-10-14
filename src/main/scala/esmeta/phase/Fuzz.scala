package esmeta.phase

import esmeta.{error => _, *}
import esmeta.cfg.CFG
import esmeta.es.util.{Coverage, withCFG}
import esmeta.es.util.fuzzer.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*

/** `fuzz` phase */
case object Fuzz extends Phase[CFG, Coverage] {
  val name = "fuzz"
  val help = "generate ECMAScript programs for fuzzing."
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Coverage = withCFG(cfg) {
    // optionally set the seed for the random number generator
    config.seed.foreach(setSeed)

    val cov = Fuzzer(
      logInterval = config.logInterval,
      debug = config.debug,
      timeLimit = config.timeLimit,
      trial = config.trial,
      synK = config.synK,
      useSens = config.useSens,
    )

    // optionally dump the generated ECMAScript programs
    for (dirname <- config.out) cov.dumpToWithDetail(dirname)

    cov
  }

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "out",
      StrOption((c, s) => c.out = Some(s)),
      "dump the generated ECMAScript programs to a given directory.",
    ),
    (
      "log-interval",
      NumOption((c, k) => c.logInterval = Some(k)),
      "turn on logging mode and set logging interval (default: 600 seconds).",
    ),
    (
      "debug",
      NumOption((c, k) =>
        if (k < 0 || k > 2) error("invalid debug level: please set 0 to 2")
        else c.debug = k,
      ),
      "turn on deug mode with level (0: no-debug, 1: partial, 2: all)",
    ),
    (
      "timeout",
      NumOption((c, k) => c.timeLimit = Some(k)),
      "set the time limit in seconds (default: 1 second).",
    ),
    (
      "trial",
      NumOption((c, k) => c.trial = Some(k)),
      "set the number of trials (default: 10000).",
    ),
    (
      "seed",
      NumOption((c, k) => c.seed = Some(k)),
      "set the specific seed for the random number generator. (default: None)",
    ),
    (
      "syn-k",
      NumOption((c, k) => c.synK = Some(k)),
      "set the specific seed for the random number generator. (default: None)",
    ),
    (
      "sens",
      BoolOption(c => c.useSens = true),
      "set the specific seed for the random number generator. (default: None)",
    ),
  )
  case class Config(
    var out: Option[String] = None,
    var logInterval: Option[Int] = Some(600),
    var debug: Int = 0,
    var timeLimit: Option[Int] = Some(1),
    var trial: Option[Int] = Some(10000),
    var seed: Option[Int] = None,
    var synK: Option[Int] = None,
    var useSens: Boolean = false,
  )
}
