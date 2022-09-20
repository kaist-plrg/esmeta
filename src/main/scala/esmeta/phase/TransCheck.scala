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
  val help = "transpiles and validates an ECMAScript program."
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Boolean =
    val filename = getFirstFilename(cmdConfig, this.name)
    val orig = readFile(filename)
    val babel = List(
      readFile(s"$RESOURCE_DIR/babel/babel@7.19.1.min.js"),
      s"let orig = `$orig`",
      readFile(s"$RESOURCE_DIR/babel/transpile.js"),
    ).mkString("\n")

    // run babel to get transpiled program
    val transpiled: String = JSEngine.runAndGetVar(babel, "transpiled").get

    // inject assertions to original program
    val injectedTest = Injector(cfg, orig, true).filterAssertion

    // replace test's script with transpiled script
    val transpiledTest = injectedTest.replaceScript(transpiled)

    // optionally dump the injected program
    for (filename <- config.out)
      dumpFile(
        name = "an assertion-transchecked ECMAScript program",
        data = transpiledTest,
        filename = filename,
      )

    // run and validate the injected program
    val failedAssertions = transpiledTest.failedAssertions
    failedAssertions.foreach({ case (a, m) => println(s"$a\n  > $m") })
    failedAssertions.isEmpty

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "out",
      StrOption((c, s) => c.out = Some(s)),
      "dump an transpiled + injected ECMAScript program to a given path.",
    ),
  )
  case class Config(
    var out: Option[String] = None,
  )
}
