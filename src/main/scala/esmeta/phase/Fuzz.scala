package esmeta.phase

import esmeta.{error => _, *}
import esmeta.cfg.CFG
import esmeta.es.util.{Coverage, withCFG}
import esmeta.es.util.fuzzer.*
import esmeta.js.Target
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import esmeta.es.util.JsonProtocol

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

    val engines: List[Target] = (config.engines match {
      case None     => List("d8", "js", "sm", "jsc")
      case Some(es) => es.split(",").toList
    }).map(e => Target(e, false))

    val transpilers: List[Target] = (config.transpilers match {
      case None =>
        List("babel", "swc", "terser", "obfuscator")
      case Some(ts) => ts.split(",").toList
    }).map(t => Target(t, true))

    val jsonProtocol = JsonProtocol(cfg)
    import jsonProtocol.given

    val bugTrieOpt =
      if config.useBugTrie then
        try {
          Some(readJson[FSTrie](s"$RESOURCE_DIR/bugs/online/bugtrie.json"))
        } catch {
          case e: Throwable =>
            print("Error while reading bugTrie: ")
            println(e.getMessage)
            None
        }
      else None

    val cov = Fuzzer(
      logInterval = config.logInterval,
      debug = config.debug,
      timeLimit = config.timeLimit,
      trial = config.trial,
      duration = config.duration,
      kFs = config.kFs,
      cp = config.cp,
      init = config.init,
      targets = engines ++ transpilers,
      checkIter = config.checkIter,
      onlineNumStdDev = config.onlineNumStdDev,
      fixedTrieOpt = bugTrieOpt,
    )

    // optionally dump the generated ECMAScript programs
    for (dirname <- config.out) cov.dumpToWithDetail(dirname)

    // cleanup resource for each target
    (engines ++ transpilers).foreach(_.cleanup)

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
      "engines",
      StrOption((c, s) => c.engines = Some(s)),
      "list of engines to test, separated by comma",
    ),
    (
      "transpilers",
      StrOption((c, s) => c.transpilers = Some(s)),
      "list of transpilers to test, separated by comma",
    ),
    (
      "use-bug-trie",
      BoolOption(c => c.useBugTrie = true),
      "use a complete trie from resources/bugs/online/.",
    ),
    (
      "threshold-std-dev",
      NumOption((c, k) => c.onlineNumStdDev = Some(k)),
      "set the threshold for update online sensitivity (-log scale) (default: 2^0).",
    ),
    (
      "checking-iteration",
      NumOption((c, k) => c.checkIter = Some(k)),
      "check update by comparing the threshold every (default: 256) iterations.",
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
    var engines: Option[String] = None,
    var transpilers: Option[String] = None,
    var checkIter: Option[Int] = Some(256),
    var onlineNumStdDev: Option[Int] = None,
    var useBugTrie: Boolean = false,
  )
}
