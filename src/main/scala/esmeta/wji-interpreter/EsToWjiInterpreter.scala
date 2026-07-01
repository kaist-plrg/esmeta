package esmeta.wji.interpreter

import esmeta.cfg.Call
import esmeta.interpreter.{Interpreter => EsInterpreter}
import esmeta.ir.{EClo, ICall}
import esmeta.state.State
import esmeta.wji.ir.Program

/** Extends ESMeta's own IR interpreter so that a literal `clo<"name">(args)`
  * call whose `name` resolves to a WJI function (instead of an ESMeta IR
  * function) runs synchronously on [[wjiInterp]] and splices the converted
  * result back in, rather than failing with `UnknownFunc`.
  *
  * This is the reverse direction of [[IrCaller]] (WJI -> ESMeta IR), sketched
  * in `IrCaller.design.md`. It is intentionally minimal, mirroring that
  * design note's "kept deliberately minimal" decision: only literal `EClo`
  * callees are intercepted (matching how `HostEnqueuePromiseJob.ir` calls
  * `WJIFunc`), and [[wjiInterp]] runs on its own private heap/globals, so
  * nesting WJI -> ESMeta IR calls back out (via [[IrCaller]]) cannot clobber
  * this interpreter's suspended [[st]].
  */
class EsToWjiInterpreter(
  st: State,
  wjiInterp: Interpreter,
  wjiProgram: Program,
) extends EsInterpreter(st):

  override def eval(call: Call): Unit = call.callInst match
    case ICall(lhs, EClo(fname, _), args) if wjiProgram.funcMap.contains(fname) =>
      val vs = args.map(eval)
      val wjiArgs = vs.map(IrCaller.fromEs)
      val wjiResult = wjiInterp.invoke(fname, wjiArgs)
      setCallResult(lhs, IrCaller.toEs(wjiResult))
    case _ => super.eval(call)
