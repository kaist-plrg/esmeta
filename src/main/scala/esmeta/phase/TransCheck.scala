package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.es.util.injector.ConformTest
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
  ): Boolean =
    val tests: Map[File, (ConformTest, ConformTest)] = {
      for (
        dir <- (cmdConfig.targets);
        files = walkTree(dir).filter(f => jsFilter(f.getName));
        _ = if (files.isEmpty) println("[Warning] No script was found");
        file <- files;
        path = file.getPath;
        script = readFile(path);
        tests = ConformTest.createTestPair(script, cfg);
        _ = if (config.debug) pprint(file, tests)
      ) yield (file, tests)
    }.toMap

    // optionally dump the compiled test
    for (dirname <- config.dir)
      dumpDir[(File, (ConformTest, ConformTest))](
        name = "conformance tests",
        iterable = tests,
        dirname = dirname,
        getName = p => s"orig-test/${p._1.getPath}",
        getData = _._2._1,
      )
      dumpDir[(File, (ConformTest, ConformTest))](
        name = "transpiled conformance tests",
        iterable = tests,
        dirname = dirname,
        getName = p => s"trans-test/${p._1.getPath}",
        getData = _._2._2,
      )
    // TODO dump other metadata

    tests.forall {
      case (_, (origTest, transTest)) =>
        origTest.isPass && transTest.isPass
    }

  def pprint(f: File, tests: (ConformTest, ConformTest)): Unit =
    val (origTest, transTest) = tests
    if (!origTest.isPass || !transTest.isPass)
      println(s"===========$f===========")
    if (!origTest.isPass) {
      println(s"orig test: ${origTest.category}")
      println(origTest.msg)
    }
    if (!transTest.isPass) {
      println(s"transpiled test: ${transTest.category}")
      println(transTest.msg)
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
