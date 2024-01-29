package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.es.util.withCFG
import esmeta.es.util.mutator.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.js.Target

/** `mutate` phase */
case object DeltaDebug extends Phase[CFG, String] {
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
    val implname = cmdConfig.targets(1)
    val impl = Target(
      implname,
      implname match {
        case s if List("d8", "js", "jsc", "sm") contains s => false
        case s if List("babel", "swc", "terser", "obfuscator") contains s =>
          true
        case _ =>
          throw new Error(
            "Given name of an ECMAScript engine or a transpiler is invalid.",
          )
      },
    )

    val ast = cfg.scriptParser.fromFile(filename)

    // For testing of this framework
    // val mutator = DeltaDebugMutator(Target)
    val mutator = RandomMutator()

    // get a mutated AST
    var minimizedAst = mutator(ast, 1).head._2
    // retry until the mutated program becomes valid
    while (!minimizedAst.valid(grammar))
      minimizedAst = mutator(ast, 1).head._2

    // get string of mutated AST
    val minimized = minimizedAst.toString(grammar)

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
