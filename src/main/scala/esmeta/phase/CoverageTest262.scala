package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.interp.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.util.BaseUtils.*
import esmeta.js.*
import esmeta.test262.*
import esmeta.test262.util.*
import esmeta.editor.util.*
import esmeta.editor.models.*

/** `coverage-test262` phase */
case object CoverageTest262 extends Phase[CFG, Unit] {
  val name = "coverage-test262"
  val help = "measure coverage of ECMA-262 (spec.html) from Test262 tests."
  def apply(
    cfg: CFG,
    globalConfig: GlobalConfig,
    config: Config,
  ): Unit = {
    val cov =
      Coverage(cfg, config.test262List, config.dump)

    if (config.astAlgo) cov.touchedAlgos
    else {
      val touched = cov.touchedNodes

      // TODO print stat?
      cfg.spec.version.foreach(v => println(s"* version: $v"))
      println("* coverage:")
      println(
        s"  - node: ${touched.size}/${cfg.nodeMap.size}" +
        ratioSimpleString(touched.size, cfg.nodeMap.size),
      )
    }
  }
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "test262-list",
      StrOption((c, s) => c.test262List = Some(s)),
      "use given test262 tests list.",
    ),
    (
      "dump",
      StrOption((c, s) => c.dump = Some(s)),
      "dump coverage data.",
    ),
    (
      "ast-algo",
      BoolOption(c => c.astAlgo = true),
      "record touched algorithms per ast node",
    ),
  )
  case class Config(
    var test262List: Option[String] = None,
    var load: Option[String] = None,
    var dump: Option[String] = Some(s"$LOG_DIR/coverage_$dateStr"),
    // var dump: Option[String] = None,
    var astAlgo: Boolean = false,
  )
}
