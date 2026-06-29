package esmeta.phase

import esmeta.*
import esmeta.util.*
import esmeta.wji.ir.Program
import esmeta.wji.interpreter.{Interpreter, InterpreterError, StubWasmHost}
import esmeta.wji.state.{Heap, WjValue}

/** `wji-interp` phase
  *
  * Runs the WJI IR interpreter on a compiled [[Program]] (replaces the old
  * `InterpreterPlayground`). By default it invokes `instantiate(moduleObject,
  * importObject)` with two empty records, using a [[StubWasmHost]] so any Wasm
  * embedding call is logged and treated as an error rather than requiring a
  * live SpecTec process.
  */
case object WjiInterp extends Phase[Program, WjValue] {
  val name = "wji-interp"
  val help = "runs the WJI IR interpreter with a stubbed WasmHost."

  def apply(
    program: Program,
    cmdConfig: CommandConfig,
    config: Config,
  ): WjValue =
    val host = StubWasmHost()
    val interp = Interpreter(program, host)
    val heap = Heap()
    val moduleObject = WjValue.Record(heap.allocRecord(Map.empty))
    val importObject = WjValue.Record(heap.allocRecord(Map.empty))

    val sep = "─" * 64
    println(s"invoke: ${config.entry}($moduleObject, $importObject)")
    println(sep)
    try
      val result =
        interp.invoke(config.entry, List(moduleObject, importObject), heap)
      println(sep)
      result
    catch
      case e: InterpreterError =>
        println(sep)
        println(s"[InterpreterError] ${e.getMessage}")
        printCallStack(interp)
        WjValue.Undef
      case e: scala.MatchError =>
        println(sep)
        println(s"[MatchError — unhandled IR node] ${e.getMessage}")
        printCallStack(interp)
        WjValue.Undef

  private def printCallStack(interp: Interpreter): Unit =
    val stack = interp.callStack.toList
    if (stack.nonEmpty)
      println("Call stack (outermost first):")
      stack.zipWithIndex.foreach((f, i) => println(s"  ${"  " * i}$f"))

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "entry",
      StrOption((c, s) => c.entry = s),
      "the WJI function to invoke (default: instantiate).",
    ),
  )
  case class Config(
    var entry: String = "instantiate",
  )
}
