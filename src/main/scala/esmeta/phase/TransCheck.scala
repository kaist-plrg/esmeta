package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
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

    // inject assertions to transpiled program
    val transChecked =
      Injector(cfg, orig, true, transpiled = Some(transpiled))

    // optionally dump the injected program
    for (filename <- config.out)
      dumpFile(
        name = "an assertion-transchecked ECMAScript program",
        data = transChecked,
        filename = filename,
      )

    // run the injected program
    JSEngine.runAndGetStdout(transChecked) match {
      case Success("")    => true
      case Success(fails) => print(fails); false
      case Failure(e)     => println(e.getMessage); false
    }

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
