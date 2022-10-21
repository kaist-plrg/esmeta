package esmeta.phase

import esmeta.*
//import esmeta.error.*
//import esmeta.es.util.injector.ConformTest.*
import esmeta.es.util.injector.Injector
import esmeta.es.Script
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.util.BaseUtils.*
import esmeta.util.{JSEngine, JSTrans}

/** `gen-test` phase */
case object GenTest
  extends Phase[
    Unit,
    (Map[String, Iterable[Script]], Map[String, Iterable[Script]]),
  ] {
  val name = "gen-test"
  val help =
    "Generate executable conform tests for ECMAScript Engines and transpilers"

  private var _config: Config = null
  private type Target = String
  private type Result = Map[Target, Iterable[Script]]

  def apply(
    _unit: Unit,
    cmdConfig: CommandConfig,
    config: Config,
  ): (Result, Result) =
    _config = config

    // collect target scripts and assertions
    val (codeDir, assertionDir) = optional {
      (cmdConfig.targets(0), cmdConfig.targets(1))
    }
      .getOrElse {
        warn(
          "No explicit directories are given. Trying to use the result from the most recent fuzz instead..",
        )
        (
          s"$FUZZ_LOG_DIR/recent/minimal",
          s"$FUZZ_LOG_DIR/recent/minimal-assertion",
        )
      }
    validate(codeDir, assertionDir)
    val names = getNames(codeDir)

    // collect target engines and transpilers
    val engines = config.engines match {
      case None => List(JSEngine.defaultCmd("d8"), JSEngine.defaultCmd("js"))
      case Some(es) => es.split(";").toList
    }
    val transpilers = config.transpilers match {
      case None     => List(JSTrans.defaultCmd("babel"))
      case Some(ts) => ts.split(";").toList
    }

    engines.zipWithIndex.foreach((engine, i) => {
      val logDir = s"$GENTEST_LOG_DIR/engine-$i"
      cleanDir(logDir)
      dumpFile(engine, s"$logDir/command.txt")
    })

    transpilers.zipWithIndex.foreach((transpiler, i) => {
      val logDir = s"$GENTEST_LOG_DIR/trans-$i"
      cleanDir(logDir)
      dumpFile(transpiler, s"$logDir/command.txt")

      if (config.debug)
        println(s" - Running transpiler $transpiler...")
      val rawDir = s"$GENTEST_LOG_DIR/raw-trans-$i"
      cleanDir(rawDir)
      JSTrans.transpileDirUsingBinary(transpiler, codeDir, rawDir)
      dumpFile(transpiler, s"$rawDir/command.txt")
    })

    names.foldLeft(
      (
        engines.map(_ -> List[Script]()).toMap,
        transpilers.map(_ -> List[Script]()).toMap,
      ),
    ) {
      case ((engineTestMap, transTestMap), name) =>
        val code = readFile(s"$codeDir/$name")
        val assertion = readFile(s"$assertionDir/$name")

        val isNormal = assertion.split(LINE_SEP).head.contains("[EXIT] normal")

        def testMaker(code: String) = {
          if (isNormal)
            List(
              code,
              libHead,
              Injector.assertionLib,
              delayHead,
              assertion,
              delayTail,
              libTail,
            ).mkString(LINE_SEP)
          else
            assertion + code
        }

        // generate engine tests
        val etest = testMaker(code)
        val updatedEngineTestMap = engineTestMap.map((e, tests) => {
          val i = engines.indexOf(e)
          val logDir = s"$GENTEST_LOG_DIR/engine-$i"
          dumpFile(etest, s"$logDir/$name")

          e -> (Script(etest, name) :: tests)
        })

        // generate trans tests
        val updatedTransTestMap = transTestMap.map((t, tests) => {
          val i = transpilers.indexOf(t)
          val rawDir = s"$GENTEST_LOG_DIR/raw-trans-$i"
          val compiledCode = readFile(s"$rawDir/$name")
          val ttest = testMaker(compiledCode)
          val logDir = s"$GENTEST_LOG_DIR/trans-$i"
          dumpFile(ttest, s"$logDir/$name")

          t -> (Script(ttest, name) :: tests)
        })

        (updatedEngineTestMap, updatedTransTestMap)
    }

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------

  // warn about correct usage
  private def warnUsage =
    throw new Exception("""
      |Usage: Please give two directory names,
      |       where the first directory contains raw code,
      |       and the second directory contains assertions.""".stripMargin)

  // get names of the files of the given directory
  private def getNames(dir: String) =
    listFiles(dir).filter(_.isFile).map(_.getName)

  // check if the given directory structure is valid
  private def validate(dir1: String, dir2: String): Unit =
    val names1 = getNames(dir1)
    val names2 = getNames(dir2)
    def assert(b: Boolean) = if (!b) warnUsage
    assert(!names1.isEmpty)
    assert(names1.toSet == names2.toSet)

  // header and footer for tests
  private val libHead = "(()=>{"
  private val libTail = "})();"
  private val delayHead = "$delay(() => {"
  private val delayTail = "});"

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
      "list of engines to test, separated by ;",
    ),
    (
      "transpilers",
      StrOption((c, s) => c.transpilers = Some(s)),
      "list of transpilers to test, separated by ;",
    ),
  )
  case class Config(
    var debug: Boolean = false,
    var engines: Option[String] = None,
    var transpilers: Option[String] = None,
  )
}