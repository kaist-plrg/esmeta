package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.state.State
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.wji.ir.Program
import esmeta.wji.interpreter.{
  EsToWjiInterpreter,
  Interpreter => WjiInterpreter,
  StubWasmHost,
}

/** `wji-eval` phase
  *
  * Runs an ECMAScript file against the real WJI IR [[Program]] compiled from
  * the WebAssembly JS API spec (via [[WjiExtract]] and [[WjiCompile]]),
  * demonstrating the reverse direction of the WJI <-> ESMeta IR bridge: the
  * hand-written intrinsic `INTRINSICS.WebAssembly.instantiate.ir` already
  * calls `clo<"instantiate">(this, %0, importObject)`, which
  * [[EsToWjiInterpreter]] routes into the WJI interpreter because the
  * compiled program defines an `"instantiate"` function; that function can in
  * turn call back into ESMeta IR abstract operations via the existing (WJI ->
  * ESMeta IR) [[esmeta.wji.interpreter.IrCaller]] fallback.
  *
  * Note: [[esmeta.wji.interpreter.IrCaller]] only marshals scalar values
  * across the ES <-> WJI boundary today, so invoking `instantiate` with its
  * real (Record-typed) arguments is expected to fail with a scalars-only
  * [[esmeta.wji.interpreter.InterpreterError]] until that boundary is
  * extended.
  */
case object WjiEval extends Phase[CFG, State] {
  val name = "wji-eval"
  val help =
    "runs ES IR against the compiled WJI IR program (and back)."

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

    val wjiInterp = WjiInterpreter(wjiProgram, StubWasmHost(), Some(cfg))

    EsToWjiInterpreter(
      cfg.init.fromFile(filename),
      wjiInterp,
      wjiProgram,
      log = config.log,
    ).result

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
