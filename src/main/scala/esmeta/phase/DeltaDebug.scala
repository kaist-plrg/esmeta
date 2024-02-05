package esmeta.phase

import esmeta.CommandConfig
import esmeta.cfg.CFG
import esmeta.es.util.withCFG
import esmeta.util.StrOption
import esmeta.util.SystemUtils.{getFirstFilename, readFile, dumpFile}
import esmeta.js.Target
import esmeta.es.util.deltadebugger.ESReducer

/** `mutate` phase */
case object DeltaDebug extends Phase[CFG, String] {

  type Script = String

  val name = "delta-debug"
  val help =
    "For given ECMAScript program and an implementation, find minimal configuration of the program which induces a failure in conform test."
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): String = withCFG(cfg) {
    val grammar = cfg.grammar
    val filename = getFirstFilename(cmdConfig, this.name)
    if cmdConfig.targets.length != 2 then
      throw new Error(
        "For this.name the name of an ECMAScript engine or a transpiler needs to be specified.",
      )
    val targetName = cmdConfig.targets(1)
    val target = Target(
      targetName,
      targetName match {
        case s if List("d8", "js", "jsc", "sm") contains s => false
        case s if List("babel", "swc", "terser", "obfuscator") contains s =>
          true
        case _ =>
          throw new Error(
            "Given name of an ECMAScript engine or a transpiler is invalid.",
          )
      },
    )

    val script = readFile(filename)

    val dd = ESReducer(script, target)
    var possiblyReduced: Option[Script] = None
    var minimized = script
    while ({
      possiblyReduced = dd.tryReduce()
      possiblyReduced.isDefined
    }) {
      minimized = possiblyReduced.get
    }

    // dump the minimized ECMAScript program
    for (filename <- config.out)
      dumpFile(
        name = "the minimized ECMAScript program",
        data = minimized,
        filename = filename,
      )

    minimized
  }

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "out",
      StrOption((c, s) => c.out = Some(s)),
      "dump the minimized ECMAScript program to a given path.",
    ),
  )
  case class Config(
    var out: Option[String] = None,
  )
}
