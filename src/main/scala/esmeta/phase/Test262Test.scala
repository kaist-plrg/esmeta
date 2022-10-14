package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.error.*
import esmeta.interpreter.*
import esmeta.state.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import esmeta.es.*
import esmeta.es.util.{Coverage, withCFG}
import esmeta.test262.{*, given}
import esmeta.test262.util.TestFilter
import java.io.File

/** `test262-test` phase */
case object Test262Test extends Phase[CFG, Summary] {
  val name = "test262-test"
  val help = "tests Test262 tests with harness files (default: tests/test262)."
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Summary = withCFG(cfg) {
    // set test mode
    TEST_MODE = true

    // get target version of Test262
    val version = Test262.getVersion(config.target)
    val test262 = Test262(version)

    // run test262 eval test in debugging mode
    if (config.debug)
      test262.evalTest(
        cmdConfig.targets,
        synK = config.synK,
        useSens = config.useSens,
      )
    // run test262 eval test
    else
      test262.evalTest(
        cmdConfig.targets,
        config.log,
        config.progress,
        config.coverage,
        config.timeLimit,
        config.synK,
        config.useSens,
      )
  }

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "debug",
      BoolOption(c => c.debug = true),
      "turn on the debugging mode.",
    ),
    (
      "log",
      BoolOption(c => c.log = true),
      "turn on logging mode.",
    ),
    (
      "target",
      StrOption((c, s) => c.target = Some(s)),
      "set the target git version of Test262 (default: current version).",
    ),
    (
      "progress",
      BoolOption(c => c.progress = true),
      "show progress bar.",
    ),
    (
      "coverage",
      BoolOption(c => c.coverage = true),
      "measure node/branch coverage in CFG of ECMA-262.",
    ),
    (
      "timeout",
      NumOption((c, k) => c.timeLimit = Some(k)),
      "set the time limit in seconds (default: no limit).",
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
    var target: Option[String] = None,
    var debug: Boolean = false,
    var log: Boolean = false,
    var coverage: Boolean = false,
    var progress: Boolean = false,
    var timeLimit: Option[Int] = None,
    var synK: Option[Int] = None,
    var useSens: Boolean = false,
  )
}
