package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.es.util.injector.Injector
import esmeta.es.Script
import esmeta.es.util.fuzzer.SelectionEval
import esmeta.es.util.withCFG
import esmeta.js.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.util.BaseUtils.*

/** `gen-test` phase */
case object PreFuzzEval
  extends Phase[
    CFG,
    Unit,
  ] {
  val name = "pre-fuzz-eval"
  val help =
    "Evaluate pre-fuzzing result using long-term fuzzing data and bug DB"

  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit = withCFG(cfg) {

    // collect target scripts and assertions
    val baseDir = optional {
      cmdConfig.targets.head
    }.getOrElse {
      warn(
        "No explicit directories are given. Trying to use the result from the most recent fuzz instead..",
      )
      s"$FUZZ_LOG_DIR/recent/"
    }
    // collect target engines and transpilers
    val engines: List[Target] = (config.engines match {
      case None     => List("d8", "js", "sm", "jsc")
      case Some(es) => es.split(",").toList
    }).map(e => Target(e, false))

    val transpilers: List[Target] = (config.transpilers match {
      case None =>
        List("babel", "swc", "terser", "obfuscator")
      case Some(ts) => ts.split(",").toList
    }).map(t => Target(t, true))

    val (bugSize1k, bugSize2k, numMinimals) =
      SelectionEval.evaluate(baseDir, engines ::: transpilers)
    println(s"average bug length compared to 1k: $bugSize1k")
    println(s"average bug length compared to 2k: $bugSize2k")
    println(s"# of minimals: $numMinimals")
    println("# of minimals in 1k: 3198")
    println("# of minimals in 2k: 16133")
  }

  def defaultConfig: Config = Config()

  val options: List[PhaseOption[Config]] = List(
    (
      "debug",
      BoolOption(c => c.debug = true),
      "turn on debug mode",
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
  )

  case class Config(
    var debug: Boolean = false,
    var engines: Option[String] = None,
    var transpilers: Option[String] = None,
  )
}
