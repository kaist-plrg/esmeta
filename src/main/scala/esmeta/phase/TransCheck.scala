package esmeta.phase

import esmeta.*
import esmeta.util.BaseUtils.warn
import esmeta.cfg.CFG
import esmeta.es.util.injector.ConformTest
import esmeta.es.util.withCFG
import esmeta.js.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import java.io.File

/** `transcheck` phase */
case object TransCheck extends Phase[CFG, Boolean] {
  val name = "transcheck"
  val help = "transpiles and validates ECMAScript programs."
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Boolean = withCFG(cfg) {
    _debug = config.debug

    val tests: Map[File, (ConformTest, ConformTest)] = {
      for {
        dir <- (cmdConfig.targets)
        files = walkTree(dir).filter(f => jsFilter(f.getName))
        _ = if (files.isEmpty) warn("No script was found")
        file <- files
        _ = debug(s"===========$file===========")
        path = file.getPath
        script = readFile(path)
        tests = ConformTest.createTestPair(script)
        _ = pprint(file, tests)
      } yield (file, tests)
    }.toMap

    // optionally dump the compiled test
    for (dirname <- config.dir)
      tests.foreach {
        case (file, (origTest, transTest)) =>
          origTest.dumpTest(s"$dirname/orig-test", file.getPath)
          transTest.dumpTest(s"$dirname/trans-test", file.getPath)
      }

    tests.forall {
      case (_, (origTest, transTest)) =>
        origTest.isPass && transTest.isPass
    }
  }

  private var _debug = false
  private def debug(msg: Any): Unit = if (_debug) println(msg)

  private def pprint(f: File, tests: (ConformTest, ConformTest)): Unit =
    val (origTest, transTest) = tests
    if (!origTest.isPass) {
      debug(s"orig test: ${origTest.category}")
      debug(origTest.msg)
    }
    if (!transTest.isPass) {
      debug(s"transpiled test: ${transTest.category}")
      debug(transTest.msg)
    }

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "dir",
      StrOption((c, s) => c.dir = Some(s)),
      "dump an transpiled + injected ECMAScript program to a given directory.",
    ),
    (
      "debug",
      BoolOption(c => c.debug = true),
      "turn on debug mode",
    ),
  )
  case class Config(
    var dir: Option[String] = None,
    var debug: Boolean = false,
  )
}
