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
import scala.collection.mutable.Map as MMap

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

    val pValueCalculator = new PValueCalculator(
      logInterval = config.logInterval,
      debug = config.debug,
      timeLimit = config.timeLimit,
      trial = config.trial,
      duration = config.duration,
      kFs = config.kFs,
      cp = config.cp,
    )

    val indepPValueMap = pValueCalculator.indepPValues
    val comboPValueTupleMap = pValueCalculator.comboPValues
    val comboPValueMap = MMap[String, MMap[String, Double]]()
    def addCombo(f1: String, f2: String, p: Double): Unit =
      comboPValueMap(f1) = comboPValueMap.getOrElse(f1, MMap())
      comboPValueMap(f1)(f2) = p
    for (((f1, f2), p) <- comboPValueTupleMap) {
      addCombo(f1, f2, p)
    }

    println(s"# of independence p-value calculations: ${indepPValueMap.size}")
    println(
      s"# of independence p-value < 0.05: ${indepPValueMap.values.count(_ < 0.05)}",
    )
    println(
      s"# of independence p-value < 0.01: ${indepPValueMap.values.count(_ < 0.01)}",
    )
    println("Dumping the calculation result")

    dumpJson(
      name = "independence pValueMap",
      data = indepPValueMap.toList.sortBy(_._2),
      filename =
        s"./p_values/independence_dur_${config.duration.getOrElse(0)}_cp_${config.cp}.json",
      space = true,
    )

    println(s"# of combo p-value calculations: ${comboPValueTupleMap.size}")
    println(
      s"# of combo p-value < 0.05: ${comboPValueTupleMap.values.count(_ < 0.05)}",
    )
    println(
      s"# of combo p-value < 0.01: ${comboPValueTupleMap.values.count(_ < 0.01)}",
    )
    dumpJson(
      name = "combo pValueMap",
      data = comboPValueMap.view
        .mapValues(_.toList)
        .toList,
      filename =
        s"./p_values/combo_${config.duration.getOrElse(0)}_cp_${config.cp}.json",
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
