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

/** `wji-reverse-demo` phase
  *
  * Runs an ECMAScript file whose IR calls a hand-written WJI function
  * (`WJIFunc`), demonstrating the reverse direction of the WJI <-> ESMeta IR
  * bridge: `HostEnqueuePromiseJob.ir` calls `clo<"WJIFunc">(42)`, which
  * [[EsToWjiInterpreter]] routes into the WJI interpreter; `WJIFunc` in turn
  * calls back into ESMeta IR's `ToBoolean` via the existing (WJI -> ESMeta IR)
  * [[esmeta.wji.interpreter.IrCaller]] fallback.
  */
case object WjiReverseDemo extends Phase[CFG, State] {
  val name = "wji-reverse-demo"
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

    EsToWjiInterpreter(cfg.init.fromFile(filename), wjiInterp, wjiProgram).result

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = Nil
  case class Config()
}
