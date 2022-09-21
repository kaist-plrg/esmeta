package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.es.ConformTest
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
        file <- walkTree(dir).filter(f => jsFilter(f.getName));
        path = file.getPath;
        script = readFile(path)
      ) yield (file, ConformTest.createTestPair(script, cfg))
    }.toMap

    // optionally dump the compiled test
    for (dirname <- config.out)
      dumpDir[(File, (ConformTest, ConformTest))](
        name = "compiled conformance tests",
        iterable = tests,
        dirname = dirname,
        getName = _._1.getPath,
        getData = _._2._2,
      )

    tests.foreach {
      case (f, (origTest, transTest)) =>
        if (!origTest.isPass || !transTest.isPass)
          println(s"===========$f===========")
        if (!origTest.isPass) {
          println("orig test: ")
          println(origTest.msg)
        }
        if (!transTest.isPass) {
          println("transpiled test: ")
          println(transTest.msg)
        }
    }

    tests.forall {
      case (_, (origTest, transTest)) =>
        origTest.isPass && transTest.isPass
    }

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "out",
      StrOption((c, s) => c.out = Some(s)),
      "dump an transpiled + injected ECMAScript program to a given directory.",
    ),
  )
  case class Config(
    var out: Option[String] = None,
  )
}
