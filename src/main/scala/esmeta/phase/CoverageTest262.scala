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

/** `coverage-test262` phase */
case object CoverageTest262 extends Phase[CFG, Unit] {
  val name = "coverage-test262"
  val help = "measure coverage of ECMA-262 (spec.html) from Test262 tests."
  def apply(
    cfg: CFG,
    globalConfig: GlobalConfig,
    config: Config,
  ): Unit = {
    // test262 configuration
    val test262 = Test262(cfg.spec)
    val test262Config =
      config.test262List.fold(test262.config)(TestFilter.fromFile)

    // progress bar
    val progress =
      ProgressBar("measure test262 coverage", test262Config.normal.zipWithIndex)

    // prepare to dump data
    config.dump.foreach { dumpDir =>
      mkdir(dumpDir)
      mkdir(s"$dumpDir/data")
      dumpFile(
        progress.seq.map(_._1.name).mkString(LINE_SEP),
        s"$dumpDir/test262-list",
      )
      dumpFile(cfg.spec.version, s"$dumpDir/ecma262-version")
      dumpFile(currentVersion(BASE_DIR), s"$dumpDir/esmeta-version")
    }

    // total covered node id
    var covered: Set[Int] = Set()

    // run test262 and measure coverage
    for (pair <- progress) {
      val (NormalConfig(name, includes), idx) = pair
      val touched =
        try {
          val (sourceText, ast) =
            test262.loadTestFromFile(s"$TEST262_TEST_DIR/$name")
          config.load match
            case None          => measureCoverage(cfg, sourceText, Some(ast))
            case Some(loadDir) => readJson[Set[Int]](s"$loadDir/data/$idx.json")
        } catch { case _: Throwable => Set() }
      config.dump.foreach { dumpDir =>
        dumpJson(touched, s"$dumpDir/data/$idx.json", noSpace = true)
      }
      covered ++= touched
    }

    // TODO print stat?
    cfg.spec.version.foreach(v => println(s"* version: $v"))
    println("* coverage:")
    println(
      s"  - node: ${covered.size}/${cfg.nodeMap.size}" +
      ratioSimpleString(covered.size, cfg.nodeMap.size),
    )
  }
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "test262-list",
      StrOption((c, s) => c.test262List = Some(s)),
      "use given test262 tests list.",
    ),
    (
      "load",
      StrOption((c, s) => c.load = Some(s)),
      "load coverage data.",
    ),
    (
      "dump",
      StrOption((c, s) => c.dump = Some(s)),
      "dump coverage data.",
    ),
  )
  case class Config(
    var test262List: Option[String] = None,
    var load: Option[String] = None,
    var dump: Option[String] = Some(s"$LOG_DIR/coverage_$dateStr"),
  )
}
