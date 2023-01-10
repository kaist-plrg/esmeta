package esmeta.phase

import esmeta.{error as _, *}
import esmeta.cfg.{CFG, Node}
import esmeta.es.util.Coverage.{Cond, CondView, NodeView}
import esmeta.es.util.{Coverage, withCFG}
import esmeta.es.util.fuzzer.*
import esmeta.state.Feature
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*

/** `selective-k fuzz` phase */
case object SKFuzz extends Phase[CFG, Coverage] {
  val name = "fuzz"
  val help = "generate ECMAScript programs for fuzzing."
  private val MAX_ATTENTION_RATIO = 0.5

  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Coverage = withCFG(cfg) {
    // optionally set the seed for the random number generator
    config.seed.foreach(setSeed)

    val nodeViewKMap = Map[Node, Int]().withDefaultValue(0)
    val condViewKMap = Map[Cond, Int]().withDefaultValue(0)

    val covPre = Fuzzer(
      logInterval = config.logInterval,
      debug = config.debug,
      timeLimit = config.timeLimit,
      trial = config.trial,
      duration = config.duration,
      kFs = config.kFs, // bias
      cp = config.cp,
    )

    // optionally dump the generated ECMAScript programs
    for (dirname <- config.out) covPre.dumpToWithDetail(dirname)

    // pre-fuzzing start ///////////////////////////////////////////////
    // TODO: edge selection
    var futNodeViewCount: Map[Feature, List[(NodeView, Int)]] = Map()
    var futCondViewCount: Map[Feature, List[(CondView, Int)]] = Map()
    var nodeViewLowSensScore: Map[NodeView, Double] = Map()
    var condViewLowSensScore: Map[CondView, Double] = Map()

    val nodePrinter =
      getPrintWriter(s"./node_count_${config.duration.getOrElse(0)}s.txt")
    val condPrinter =
      getPrintWriter(s"./cond_count_${config.duration.getOrElse(0)}s.txt")

    for {
      (nodeView, count) <- covPre.nodeViewCount;
      (featureStack, _, _) <- nodeView.view
    } yield {
      if (featureStack.nonEmpty) {
        val last = featureStack.last
        futNodeViewCount += last -> ((nodeView, count) :: futNodeViewCount
          .getOrElse(last, List()))
      }
    }
    for {
      (condView, count) <- covPre.condViewCount;
      (featureStack, _, _) <- condView.view
    } yield {
      if (featureStack.nonEmpty) {
        val last = featureStack.last
        futCondViewCount += last -> ((condView, count) :: futCondViewCount
          .getOrElse(last, List()))
      }
    }
    futNodeViewCount.foreach {
      case (_, countList) => {
        var sortedCountList = countList.sortBy(_._2).reverse
        val totalCount = countList.map(_._2).sum
        var acc = 0
        // TODO: parameterize 2
        while (acc < totalCount * MAX_ATTENTION_RATIO) {
          sortedCountList match {
            case h :: t =>
              sortedCountList = t
              acc += h._2
              nodeViewLowSensScore +=
                h._1 -> (nodeViewLowSensScore.getOrElse(
                  h._1,
                  0.0,
                ) + h._2 / totalCount)
            case _ => acc = totalCount
          }
        }
      }
    }

    futCondViewCount.foreach {
      case (_, countList) => {
        var sortedCountList = countList.sortBy(_._2).reverse
        val totalCount = countList.map(_._2).sum
        var acc = 0
        while (acc < totalCount * MAX_ATTENTION_RATIO) {
          sortedCountList match {
            case h :: t =>
              sortedCountList = t
              acc += h._2
              condViewLowSensScore +=
                h._1 -> (condViewLowSensScore.getOrElse(
                  h._1,
                  0.0,
                ) + h._2 / totalCount)
            case _ => acc = totalCount
          }
        }
      }
    }

    nodeViewLowSensScore.toList.sortBy(_._2).foreach {
      case (nodeView, score) => {
        nodePrinter.print(f"$nodeView%200s")
        nodePrinter.println(f"  $score%04f")
      }
    }
    condViewLowSensScore.toList.sortBy(_._2).foreach {
      case (condView, score) =>
        condPrinter.print(f"$condView%200s")
        condPrinter.println(f"  $score%04f")
    }
    nodePrinter.close()
    condPrinter.close()
    // pre-fuzzing end //////////////////////////////////////////////

    covPre
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
  )
}
