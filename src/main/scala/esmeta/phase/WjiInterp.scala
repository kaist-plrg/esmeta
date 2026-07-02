package esmeta.phase

import esmeta.*
import esmeta.util.*
import esmeta.cfg.CFG
import esmeta.cfgBuilder.CFGBuilder
import esmeta.error.ESMetaError
import esmeta.interpreter.{Interpreter => EsInterpreter}
import esmeta.ir.{Local, Program}
import esmeta.state.*
import esmeta.wji.bridge.SpecTecWasmHost
import esmeta.wji.bridge.process.SpecTecProcess
import esmeta.wji.bridge.rpc.JsonRpcConnection
import scala.collection.mutable.{Map => MMap}

/** `wji-interp` phase
  *
  * Runs the compiled WJI [[Program]] on ESMeta's own unified
  * `esmeta.interpreter.Interpreter` (replaces the old `InterpreterPlayground` /
  * WJI-only interpreter, now that WJI algorithms compile to real `esmeta.ir`
  * functions). By default it invokes `instantiate(moduleObject, importObject)`
  * with two empty records. Wasm embedding calls (`ICallEmbed`) are dispatched
  * to a live [[SpecTecWasmHost]] over JSON-RPC, so this requires a live SpecTec
  * process on `PATH` (see [[SpecTecProcess]]).
  */
case object WjiInterp extends Phase[Program, Value] {
  val name = "wji-interp"
  val help = "runs the compiled WJI program on the unified interpreter."

  def apply(
    program: Program,
    cmdConfig: CommandConfig,
    config: Config,
  ): Value =
    val cfg = CFGBuilder(program)
    given CFG = cfg
    val func = cfg.getFunc(config.entry)
    val heap = Heap()
    val moduleObject = heap.allocRecord("ModuleObject")
    val importObject = heap.allocRecord("ImportObject")
    val locals: MMap[Local, Value] =
      MMap.from(func.params.map(_.lhs).zip(List(moduleObject, importObject)))
    val st =
      State(cfg, Context(func, locals), globals = MMap.empty, heap = heap)

    val process = SpecTecProcess.start()
    val connection = JsonRpcConnection.stdio(process)
    val host = SpecTecWasmHost(connection)

    val sep = "─" * 64
    println(s"invoke: ${config.entry}($moduleObject, $importObject)")
    println(sep)
    try
      EsInterpreter(st, wasmHost = Some(host))
      println(sep)
      st.globals.getOrElse(GLOBAL_RESULT, Undef)
    catch
      case e: ESMetaError =>
        println(sep)
        println(s"[${e.getClass.getSimpleName}] ${e.getMessage}")
        printCallStack(st)
        Undef
      case e: scala.MatchError =>
        println(sep)
        println(s"[MatchError — unhandled IR node] ${e.getMessage}")
        printCallStack(st)
        Undef
    finally connection.close()

  private def printCallStack(st: State): Unit =
    val stack =
      (st.context.name :: st.callStack.map(_.context.name)).reverse
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
