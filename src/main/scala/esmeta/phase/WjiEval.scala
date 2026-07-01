package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.state.State
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.wji.ir.*
import esmeta.wji.interpreter.{
  EsToWjiInterpreter,
  Interpreter => WjiInterpreter,
  StubWasmHost,
}

/** `wji-eval` phase
  *
  * Runs an ECMAScript file whose IR calls a hand-written WJI function
  * (`WJIFunc`), demonstrating the reverse direction of the WJI <-> ESMeta IR
  * bridge: `HostEnqueuePromiseJob.ir` calls `clo<"WJIFunc">(42.0f)`, which
  * [[EsToWjiInterpreter]] routes into the WJI interpreter; `WJIFunc` in turn
  * calls back into ESMeta IR's `ToBoolean` via the existing (WJI -> ESMeta IR)
  * [[esmeta.wji.interpreter.IrCaller]] fallback.
  */
case object WjiEval extends Phase[CFG, State] {
  val name = "wji-eval"
  val help =
    "runs ES IR that calls into a hand-written WJI function (and back)."

  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): State =
    val filename = getFirstFilename(cmdConfig, this.name)

    val wjiFunc = Func(
      name = "WJIFunc",
      params = List(Param(Name("arg"))),
      body = ISeq(
        List(
          IPrint(ERef(Name("arg"))),
          ICall(Name("b"), EClo("ToBoolean"), List(ERef(Name("arg")))),
          IPrint(ERef(Name("b"))),
          IReturn(ERef(Name("b"))),
        ),
      ),
    )
    val wjiProgram = Program(List(wjiFunc))
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
      "log",
      BoolOption(_.log = _),
      "turn on logging mode.",
    ),
  )
  case class Config(
    var log: Boolean = false,
  )
}
