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

    val nodeKMapOpt = if (config.preFuzzIter != 0) {
      try {
        val nodeKMap =
          readJson[List[(String, Int)]](
            s"./k_selection/node_attn${config.attentionPercent}_cut${config.cutPercent}_iter${config.preFuzzIter}_dur${config.preFuzzDuration}.json",
          ).toMap
        println(s"read ${nodeKMap.size} node k-selections.")
        Some(nodeKMap)
      } catch {
        case e: Throwable =>
          println(e.getMessage)
          None
      }
    } else None

    val condKMapOpt = if (config.preFuzzIter != 0) {
      try {
        val condKMap = readJson[List[(String, Int)]](
          s"./k_selection/cond_attn${config.attentionPercent}_cut${config.cutPercent}_iter${config.preFuzzIter}_dur${config.preFuzzDuration}.json",
        ).toMap
        println(s"read ${condKMap.size} condition k-selections.")
        Some(condKMap)
      } catch {
        case e: Throwable =>
          println(e.getMessage)
          None
      }
    } else None

    val indepPValueMapOpt = if (config.pValue) {
      try {
        val pValueMap = readJson[List[(String, Double)]](
          s"./p_values/independence_dur_${config.duration.getOrElse(0)}_cp_${config.cp}.json",
        ).toMap
        println(s"read ${pValueMap.size} indep p-values.")
        Some(pValueMap)
      } catch {
        case e: Throwable =>
          println(e.getMessage)
          None
      }
    } else None

    val comboPValueMapOpt = if (config.pValue) {
      try {
        val pValueMap = readJson[List[((String, String), Double)]](
          s"./p_values/combo_${config.duration.getOrElse(0)}_cp_${config.cp}.json",
        ).toMap
        println(s"read ${pValueMap.size} combo p-values.")
        Some(pValueMap)
      } catch {
        case e: Throwable =>
          println(e.getMessage)
          None
      }
    } else None

    val cov = Fuzzer(
      logInterval = config.logInterval,
      debug = config.debug,
      timeLimit = config.timeLimit,
      trial = config.trial,
      duration = config.duration,
      kFs = config.kFs,
      cp = config.cp,
      init = config.init,
      nodeViewKMap = nodeKMapOpt.getOrElse(Map()).withDefaultValue(0),
      condViewKMap = condKMapOpt.getOrElse(Map()).withDefaultValue(0),
      indepPValueMapOpt = indepPValueMapOpt,
      comboPValueMapOpt = comboPValueMapOpt,
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
      "turn on debug mode with level (0: no-debug, 1: partial, 2: all)",
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
      "set the maximum duration for fuzzing (default: INF)",
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
      "init",
      StrOption((c, s) => c.init = Some(s)),
      "explicitly use the given init pool",
    ),
    (
      "pre-fuzz-iter",
      NumOption((c, k) => c.preFuzzIter = k),
      "use pre-fuzzing data to select sensitivity (default: 0).",
    ),
    (
      "pre-fuzz-duration",
      NumOption((c, k) => c.preFuzzDuration = k),
      "use pre-fuzzing data to select sensitivity (default: 60).",
    ),
    (
      "attention-percent",
      NumOption((c, k) => c.attentionPercent = k),
      "give attention to top {given number}% of previous features (default: 50)",
    ),
    (
      "cut-percent",
      NumOption((c, k) => c.cutPercent = k),
      "cut features over {given number}% of the average (default: 100)",
    ),
    (
      "p-value",
      BoolOption(c => c.pValue = true),
      "use p-value data for tunneling.",
    ),
  )

  case class Config(
    var out: Option[String] = None,
    var logInterval: Option[Int] = Some(600),
    var debug: Int = 0,
    var timeLimit: Option[Int] = Some(1),
    var trial: Option[Int] = None,
    var duration: Option[Int] = None,
    var seed: Option[Int] = None,
    var kFs: Int = 0,
    var cp: Boolean = false,
    var init: Option[String] = None,
    var preFuzzIter: Int = 0,
    var preFuzzDuration: Int = 60,
    var attentionPercent: Int = 50,
    var cutPercent: Int = 100,
    var pValue: Boolean = false,
  )
}
