package esmeta.phase

import esmeta.*
import esmeta.util.*
import esmeta.wji.lang.Algorithm
import esmeta.wji.compiler.Compiler
import esmeta.wji.compiler.desugar.Desugar
import esmeta.wji.ir.{IrPrinter, Program}

/** `wji-compile` phase
  *
  * Desugars the extracted WJI algorithms and compiles them to a WJI IR
  * [[Program]]. Optionally prints the rendered IR (replaces the old `printIR`
  * entry point).
  */
case object WjiCompile extends Phase[List[Algorithm], Program] {
  val name = "wji-compile"
  val help = "compiles WJI algorithms to a WJI IR program."

  def apply(
    algorithms: List[Algorithm],
    cmdConfig: CommandConfig,
    config: Config,
  ): Program =
    val program = Compiler.compile(Desugar.run(algorithms))
    if (config.log)
      println(s"${program.funcs.size} func(s)")
      println(IrPrinter.render(program))
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
