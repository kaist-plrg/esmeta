package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.es.ConformTest
import esmeta.es.util.injector.Injector
import esmeta.util.*
import esmeta.util.SystemUtils.*
import scala.util.{Success, Failure}

/** `transcheck` phase */
case object TransCheck extends Phase[CFG, Boolean] {
  val name = "transcheck"
  val help = "transpiles and validates ECMAScript programs."
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Boolean =
    val files = for (
      dir <- (cmdConfig.targets);
      file <- walkTree(dir).filter(f => jsFilter(f.getName))
    ) yield file

    val results = files
      .map(file =>
        val filename = file.getPath
        val orig = readFile(filename)

        // run babel to get transpiled program
        val transpiled = Babel.transpile(orig)

        // inject assertions to original program
        val injectedTest = Injector(cfg, orig, true)

        // replace test's script with transpiled script
        val transpiledTest =
          injectedTest.filterAssertion.replaceScript(transpiled)

        // optionally dump the injected program
        for (dirname <- config.out)
          dumpFile(
            name = "a compiled conformance test",
            data = transpiledTest,
            filename = s"$dirname/$filename",
          )

        // run and validate the injected program
        (
          file,
          (injectedTest.failedAssertions, transpiledTest.failedAssertions),
        ),
      )
      .toMap

    results.foreach {
      case (f, (origFails, transFails)) =>
        println(s"===========$f===========")
        if (!origFails.isEmpty) {
          println("orig test: ")
          origFails.foreach { case (a, m) => println(s"$a\n  > $m") }
        }
        if (!transFails.isEmpty) {
          println("transpiled test: ")
          transFails.foreach { case (a, m) => println(s"$a\n  > $m") }
        }
    }

    results.forall {
      case (_, (f1, f2)) =>
        f1.isEmpty && f2.isEmpty
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
