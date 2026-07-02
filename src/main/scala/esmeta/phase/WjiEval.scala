package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.cfgBuilder.CFGBuilder
import esmeta.interpreter.{Interpreter => EsInterpreter}
import esmeta.ir.Program
import esmeta.state.State
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.wji.bridge.SpecTecWasmHost
import esmeta.wji.bridge.process.SpecTecProcess
import esmeta.wji.bridge.rpc.JsonRpcConnection

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
    extractConfig.log = config.log
    val algorithms = WjiExtract((), cmdConfig, extractConfig)

    val compileConfig = WjiCompile.defaultConfig
    compileConfig.log = config.log
    val wjiProgram = WjiCompile(algorithms, cmdConfig, compileConfig)

    val merged =
      Program(cfg.program.funcs ++ wjiProgram.funcs, cfg.program.spec)
    val mergedCfg = CFGBuilder(merged)


    val process = SpecTecProcess.start()
    val connection = JsonRpcConnection.stdio(process)
    val host = SpecTecWasmHost(connection)

    EsInterpreter(
      mergedCfg.init.fromFile(filename),
      log = config.log,
      wasmHost = Some(host),
    )

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
  )
  case class Config(
    var filter: String = "",
    var log: Boolean = false,
  )
}
