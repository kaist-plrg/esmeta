package esmeta.wji

import esmeta.ESMetaTest
import esmeta.cfg.CFG
import esmeta.cfgBuilder.CFGBuilder
import esmeta.interpreter.{Interpreter => EsInterpreter}
import esmeta.ir.{NormalInst, Program}
import esmeta.state.State
import esmeta.wji.bridge.host.WasmHost
import esmeta.wji.compiler.Compiler
import esmeta.wji.compiler.lowering.Lowering
import esmeta.wji.extractor.Extractor

/** Support for running `.js` fixtures under `tests/wji` end to end — mirrors
  * `esmeta.es.ESTest`'s role, but merges the WJI IR program into the shared
  * mainline CFG and threads a fresh [[WasmHost]]/SpecTec connection through the
  * [[EsInterpreter]] per fixture, replicating [[esmeta.phase.WjiEval]]'s
  * pipeline exactly.
  */
object WjiTest:

  /** the WJI IR program merged into the SAME mainline CFG every
    * `esmeta.es`/`esmeta.ir` test already shares (`ESMetaTest.cfg`) — built
    * once (JVM-wide `lazy val`) and reused across every fixture, rather than
    * re-extracting/re-compiling the spec per test.
    */
  lazy val mergedCfg: CFG =
    val spec = Extractor()
    // must happen before the interpreter runs (see esmeta.wji.spec.Spec's doc)
    spec.registerInterfaceTypes()
    val wjiProgram = Compiler.compile(Lowering.run(spec.algorithms))
    val mainline = ESMetaTest.cfg.program
    CFGBuilder(Program(mainline.funcs ++ wjiProgram.funcs, mainline.spec))

  /** same shape as `esmeta.es.ESTest.CheckAfter`, plus a `wasmHost` threaded to
    * the interpreter — kept as a separate small class here rather than reusing
    * `ESTest.CheckAfter` directly, since `esmeta.es` must not depend on
    * `esmeta.wji` (the dependency only ever goes the other way).
    */
  private class CheckAfter(
    st: State,
    checkAfter: List[NormalInst],
    wasmHost: Option[WasmHost],
  ) extends EsInterpreter(st, wasmHost = wasmHost):
    override lazy val result: State =
      while (step) {}
      for (assert <- checkAfter) super.eval(assert)
      st

  /** runs a single `.js` fixture: fresh `State`, fresh SpecTec process for the
    * duration of the run, always torn down afterward.
    */
  def evalFile(jsPath: String, checkAfter: List[NormalInst] = Nil): State =
    val st = mergedCfg.init.fromFile(jsPath)
    val (host, connection) = Initialize(st)
    try new CheckAfter(st, checkAfter, Some(host)).result
    finally connection.close()
