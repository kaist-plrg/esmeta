package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.cfgBuilder.CFGBuilder
import esmeta.error.ESMetaError
import esmeta.interpreter.{Interpreter => EsInterpreter}
import esmeta.ir.Program
import esmeta.state.State
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.wji
import esmeta.wji.util.AlgoCallStack

/** `wji-eval` phase
  *
  * Runs an ECMAScript file against the real WJI IR [[Program]] compiled from
  * the WebAssembly JS API spec (via [[WjiExtract]] and [[WjiCompile]]), merged
  * into the same `CFG` as the ES spec's own functions and executed by the SAME
  * `esmeta.interpreter.Interpreter` — the hand-written intrinsic
  * `INTRINSICS.WebAssembly.instantiate.ir` already calls
  * `clo<"instantiate">(this, %0, importObject)`, which now resolves directly
  * against the merged CFG's `fnameMap`, exactly like any other ES-to-ES call.
  * No separate WJI interpreter, heap, or ES <-> WJI value-conversion bridge is
  * needed any more.
  */
case object WjiEval extends Phase[CFG, State] {
  val name = "wji-eval"
  val help =
    "runs ES IR against the compiled WJI IR program (merged into the same CFG)."

  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): State =
    val filename = getFirstFilename(cmdConfig, this.name)

    val extractConfig = WjiExtract.defaultConfig
    extractConfig.filter = config.filter
    val spec = WjiExtract((), cmdConfig, extractConfig)

    val compileConfig = WjiCompile.defaultConfig
    val wjiProgram = WjiCompile(spec, cmdConfig, compileConfig)

    val merged =
      Program(cfg.program.funcs ++ wjiProgram.funcs, cfg.program.spec)
    val mergedCfg = CFGBuilder(merged)

    val st = mergedCfg.init.fromFile(filename)
    val (host, connection) = wji.Initialize(st, spec)
    val sep = "─" * 64
    try
      EsInterpreter(
        st,
        log = config.log,
        timeLimit = config.timeout,
        wasmHost = Some(host),
      )
    catch
      case e: (ESMetaError | NotImplementedError) =>
        println(sep)
        println(s"[${e.getClass.getSimpleName}] ${e.getMessage}")
        AlgoCallStack.printCallStack(st)
        throw e
      case e: scala.MatchError =>
        println(sep)
        println(s"[MatchError — unhandled IR node] ${e.getMessage}")
        AlgoCallStack.printCallStack(st)
        throw e
      case e: java.util.concurrent.TimeoutException =>
        println(sep)
        println(s"[Timeout] exceeded ${config.timeout.getOrElse("?")}s")
        AlgoCallStack.printCallStack(st)
        throw e
    finally connection.close()

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "filter",
      StrOption((c, s) => c.filter = s),
      "only compile WJI algorithms whose name or id contains this substring.",
    ),
    (
      "log",
      BoolOption(_.log = _),
      "turn on logging mode.",
    ),
    (
      "timeout",
      NumOption((c, k) => c.timeout = Some(k)),
      "set the time limit in seconds (default: no limit) -- checked "
      + "periodically by the interpreter itself, so a slow test times out "
      + "cleanly (SpecTec connection closed via `finally`) instead of "
      + "hanging the process.",
    ),
  )
  case class Config(
    var filter: String = "",
    var log: Boolean = false,
    var timeout: Option[Int] = None,
  )
}
