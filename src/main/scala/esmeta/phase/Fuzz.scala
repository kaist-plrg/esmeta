package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.es.util.Coverage
import esmeta.es.util.fuzzer.*
import esmeta.util.*
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
    val cov = Fuzzer(
      cfg = cfg,
      log = config.log,
      timeLimit = config.timeLimit,
      trial = config.trial,
      conformtest = config.conformtest,
    )

    // dump the generated ECMAScript programs
    for (dirname <- config.out) cov.dumpTo(dirname, withScripts = true)

    cov

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "out",
      StrOption((c, s) => c.out = Some(s)),
      "dump the generated ECMAScript programs to a given directory.",
    ),
    (
      "log",
      NumOption((c, k) => c.log = Some(k)),
      "turn on logging mode and set logging interval (default: 600 seconds).",
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
      "conformtest",
      BoolOption(c => c.conformtest = true),
      "do conformance test during fuzzing",
    ),
  )
  case class Config(
    var out: Option[String] = None,
    var log: Option[Int] = Some(600),
    var timeLimit: Option[Int] = Some(1),
    var trial: Option[Int] = Some(10000),
    var conformtest: Boolean = false,
  )
}
