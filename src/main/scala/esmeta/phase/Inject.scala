package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.es.util.withCFG
import esmeta.es.util.injector.Injector
import esmeta.interpreter.Interpreter
import esmeta.es.*
import esmeta.state.*
import esmeta.test262.*
import esmeta.util.*
import esmeta.util.SystemUtils.*

/** `inject` phase */
case object Inject extends Phase[CFG, String] {
  val name = "inject"
  val help = "injects assertions to check final state of an ECMAScript file."
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): String = withCFG(cfg) {
    val filename = getFirstFilename(cmdConfig, this.name)
    val injected = Injector.fromFile(filename, config.defs, config.log)

    // dump the assertion-injected ECMAScript program
    for (filename <- config.out)
      dumpFile(
        name = "an assertion-injected ECMAScript program",
        data = injected,
        filename = filename,
      )

    injected.toString
  }

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "defs",
      BoolOption(c => c.defs = true),
      "prepend definitions of helpers for assertions.",
    ),
    (
      "out",
      StrOption((c, s) => c.out = Some(s)),
      "dump an assertion-injected ECMAScript program to a given path.",
    ),
    (
      "log",
      BoolOption(c => c.log = true),
      "turn on logging mode.",
    ),
  )
  case class Config(
    var defs: Boolean = false,
    var out: Option[String] = None,
    var log: Boolean = false,
  )
}
