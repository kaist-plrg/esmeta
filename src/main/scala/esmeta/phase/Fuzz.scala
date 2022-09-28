package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.es.util.Coverage
import esmeta.es.util.fuzzer.*
import esmeta.util.*
import esmeta.util.BaseUtils.setSeed
import esmeta.util.SystemUtils.*

/** `fuzz` phase */
case object Fuzz extends Phase[CFG, Coverage] {
  val name = "fuzz"
  val help = "generate ECMAScript programs for fuzzing."
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Coverage =
    // optionally set the seed for the random number generator
    config.seed.foreach(setSeed)

    val cov = Fuzzer(
      cfg = cfg,
      logInterval = config.logInterval,
      debug = config.debug,
      timeLimit = config.timeLimit,
      trial = config.trial,
      conformTest = config.conformTest,
    )

    // optionally dump the generated ECMAScript programs
    for (dirname <- config.out) cov.dumpToWithDetail(dirname)

    cov

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
      BoolOption(c => c.debug = true),
      "turn on deug mode",
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
      "conform-test",
      BoolOption(c => c.conformTest = true),
      "do conformance test during fuzzing",
    ),
    (
      "seed",
      NumOption((c, k) => c.seed = Some(k)),
      "set the specific seed for the random number generator. (default: None)",
    ),
  )
  case class Config(
    var out: Option[String] = None,
    var logInterval: Option[Int] = Some(600),
    var debug: Boolean = false,
    var timeLimit: Option[Int] = Some(1),
    var trial: Option[Int] = Some(10000),
    var conformTest: Boolean = false,
    var seed: Option[Int] = None,
  )
}
