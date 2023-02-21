package esmeta.phase

import esmeta.cfg.{CFG, Node}
import esmeta.es.util.Coverage.{Cond, CondView, NodeView}
import esmeta.es.util.fuzzer.*
import esmeta.es.util.{Coverage, withCFG}
import esmeta.state.Feature
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import esmeta.{error as _, *}

/** `selective-k fuzz` phase */
case object CalculateP extends Phase[CFG, Unit] {
  val name = "calculate-p"
  val help =
    "generate ECMAScript programs for fuzzing (using selective sensitivity)."

  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit = withCFG(cfg) {
    // optionally set the seed for the random number generator
    config.seed.foreach(setSeed)

    println(config.duration)
    val pValueMap = PValueCalculator.getPValues(
      logInterval = config.logInterval,
      debug = config.debug,
      timeLimit = config.timeLimit,
      trial = config.trial,
      duration = config.duration,
      kFs = config.kFs,
      cp = config.cp,
//      preFuzzIter = config.preFuzzIter,
    )

    println(s"# of p-value calculations: ${pValueMap.size}")
    println(s"# of p-value < 0.05: ${pValueMap.values.count(_ < 0.05)}")
    println(s"# of p-value < 0.01: ${pValueMap.values.count(_ < 0.01)}")
    println("Dumping the calculation result")

    dumpJson(
      name = "pValueMap",
      data = pValueMap.toList.sortBy(_._2),
      filename =
        s"./p_values/dur_${config.duration.getOrElse(0)}_cp_${config.cp}.json",
      space = true,
    )
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
      "set the number of trials (default: INF).",
    ),
    (
      "duration",
      NumOption((c, k) => c.duration = Some(k)),
      "set the maximum duration for fuzzing (default: 60)",
    ),
    (
      "seed",
      NumOption((c, k) => c.seed = Some(k)),
      "set the specific seed for the random number generator (default: None).",
    ),
    (
      "k-fs",
      NumOption((c, k) => c.kFs = k),
      "set the k-value for feature sensitivity (default: 0).",
    ),
    (
      "cp",
      BoolOption(c => c.cp = true),
      "turn on the call-path mode (default: false) (meaningful if k-fs > 0).",
    ),
    (
      "iter",
      NumOption((c, k) => c.preFuzzIter = k),
      "set the number of iterations for selective feature sensitivity (default: 0).",
    ),
  )

  case class Config(
    var out: Option[String] = None,
    var logInterval: Option[Int] = Some(600),
    var debug: Int = 0,
    var timeLimit: Option[Int] = Some(1),
    var trial: Option[Int] = None,
    var duration: Option[Int] = Some(60),
    var seed: Option[Int] = None,
    var kFs: Int = 0,
    var cp: Boolean = false,
    var preFuzzIter: Int = 0,
    var attentionPercent: Int = 50,
    var cutPercent: Int = 100,
  )
}
