package esmeta.phase

import esmeta.*
import esmeta.util.*
import esmeta.wji.lang.Algorithm
import esmeta.wji.compiler.Compiler
import esmeta.wji.compiler.lowering.Lowering
import esmeta.ir.Program

/** `wji-compile` phase
  *
  * Lowers the extracted WJI algorithms and compiles them to a real ESMeta IR
  * [[Program]] (the same `esmeta.ir` shape the ECMA-262 spec compiles to, so it
  * can later be merged into the same `CFG`). Optionally prints the rendered IR
  * (replaces the old `printIR` entry point).
  */
case object WjiCompile extends Phase[List[Algorithm], Program] {
  val name = "wji-compile"
  val help = "compiles WJI algorithms to a WJI IR program."

  def apply(
    algorithms: List[Algorithm],
    cmdConfig: CommandConfig,
    config: Config,
  ): Program =
    val program = Compiler.compile(Lowering.run(algorithms))
    if (config.log)
      println(s"${program.funcs.size} func(s)")
      println(program.toString())
    program

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "log",
      BoolOption(_.log = _),
      "print the compiled WJI IR program.",
    ),
  )
  case class Config(
    var log: Boolean = false,
  )
}
