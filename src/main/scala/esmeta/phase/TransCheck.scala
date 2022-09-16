package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.injector.Injector
import esmeta.interpreter.Interpreter
import esmeta.es.*
import esmeta.state.*
import esmeta.test262.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.util.JSEngine

/** `transcheck` phase */
case object TransCheck extends Phase[CFG, Boolean] {
  val name = "transcheck"
  val help = "Validate the transpilation of the given ECMAScript file."
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Boolean =
    val filename = getFirstFilename(cmdConfig, this.name)

    val transpiled: String = "asdfsadf"
    val transChecked =
      Injector.fromFile(cfg, filename, transpiled = Some(transpiled))
    println(transChecked)

    // dump the assertion-transchecked ECMAScript program
    for (filename <- config.out)
      dumpFile(
        name = "an assertion-transchecked ECMAScript program",
        data = transChecked,
        filename = filename,
      )

//    val result = JSEngine.run(transChecked)
    false
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
