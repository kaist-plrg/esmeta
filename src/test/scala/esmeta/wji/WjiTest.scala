package esmeta.wji

import esmeta.ESMetaTest
import esmeta.cfg.CFG
import esmeta.cfgBuilder.CFGBuilder
import esmeta.interpreter.{Interpreter => EsInterpreter}
import esmeta.ir.{Global, Program}
import esmeta.state.{Bool, State, Str, Value}
import esmeta.wji.bridge.host.WasmHost
import esmeta.wji.bridge.rpc.JsonRpcConnection
import esmeta.wji.compiler.Compiler
import esmeta.wji.compiler.lowering.Lowering
import esmeta.wji.extractor.Extractor
import esmeta.wji.spec.Spec
import org.scalatest.Assertions.*
import scala.util.{Success, Try}

/** Support for running `.js` test cases under `tests/wji/manual` end to end —
  * mirrors `esmeta.es.ESTest`'s role, but merges the WJI IR program into the
  * shared mainline CFG and threads a fresh [[WasmHost]]/SpecTec connection
  * through the [[EsInterpreter]] per test case, replicating
  * [[esmeta.phase.WjiEval]]'s pipeline exactly.
  */
object WjiTest:

  lazy val spec = Extractor()

  /** the WJI IR program merged into the SAME mainline CFG every
    * `esmeta.es`/`esmeta.ir` test already shares (`ESMetaTest.cfg`) — built
    * once (JVM-wide `lazy val`) and reused across every test case, rather than
    * re-extracting/re-compiling the spec per test.
    */
  lazy val mergedCfg: CFG =
    // must happen before the interpreter runs (see esmeta.wji.spec.Spec's doc)
    spec.registerDefinitionTypes()
    val wjiProgram = Compiler.compile(Lowering.run(spec.algorithms))
    val mainline = ESMetaTest.cfg.program
    CFGBuilder(Program(mainline.funcs ++ wjiProgram.funcs, mainline.spec))

  /** reads `globalThis.__wjiOk` straight out of `st`, rather than through an
    * IR-level `assert`: `IAssert`'s evaluation is wrapped in `optional(...)`
    * (`esmeta.util.BaseUtils`), which swallows *any* `Throwable` — not just the
    * "not yet compiled" case it's meant for — as a deliberate way to skip
    * assertions referencing unmechanized spec text. That silently turns "the
    * test case never set `__wjiOk`" (`__MAP__` has no such key, so the field
    * read itself throws `InvalidObjField`) into "assertion skipped, treated as
    * passing" — exactly the failure mode this check exists to catch, so an IR
    * `assert` can't safely express it. Every test case must set this global
    * itself, as its very last action (synchronously or from inside a
    * `.then()`/callback), once every check it performs has passed — see
    * `tests/wji/manual/README.md`. Plain `throw` alone isn't enough for
    * assertions inside a `.then()` callback: per the `NewPromiseReactionJob`
    * spec algorithm, a `.then()` handler's throw gets absorbed into rejecting
    * that call's own (usually unobserved) derived promise, so it never becomes
    * an abrupt completion of `RunJobs` and thus never shows up in
    * `GLOBAL_RESULT` (verified empirically — a bare `Promise.resolve().then(()
    * => { throw ... })` still leaves `@RESULT -> undefined`). `throw` is still
    * the right tool for synchronous checks (and reads naturally to a human or a
    * real JS engine); this flag is the only reliable way to additionally
    * observe an async one here.
    */
  private def wjiOk(st: State): Try[Value] =
    for
      realm <- st.get(Global("REALM"))
      globalObj <- st.get(realm, Str("GlobalObject"))
      map <- st.get(globalObj, Str("__MAP__"))
      prop <- st.get(map, Str("__wjiOk"))
      value <- st.get(prop, Str("Value"))
    yield value

  /** same shape as `esmeta.es.ESTest.CheckAfter`, minus the `checkAfter` list
    * (unneeded now that the one thing it was checking, `__wjiOk`, is read
    * directly instead — see [[wjiOk]]), plus a `wasmHost` threaded to the
    * interpreter — kept as a separate small class here rather than reusing
    * `ESTest.CheckAfter` directly, since `esmeta.es` must not depend on
    * `esmeta.wji` (the dependency only ever goes the other way).
    */
  private class RunToCompletion(st: State, wasmHost: Option[WasmHost])
    extends EsInterpreter(st, wasmHost = wasmHost):
    override lazy val result: State =
      while (step) {}
      st

  /** runs a single `.js` test case against an already-running SpecTec
    * `connection` (built once per test-suite run by the caller — see
    * `EvalSpec`'s `beforeAll` — rather than once per test case, since process
    * spawn dominates per-test wall time: ~10s vs ~1-2s of actual test
    * execution). Builds a fresh `State`/[[esmeta.wji.bridge.host.WasmHost]] for
    * this one test via `Initialize(st, connection)`, but does *not* close
    * `connection` — the caller owns its lifecycle across the whole suite, and
    * must treat any exception escaping this method as a sign `connection` may
    * be left desynced (see `EvalSpec`'s catch-and-respawn) rather than assume
    * it's still safe to reuse. Always checks that the test case itself set
    * `__wjiOk` (see above).
    */
  def evalFile(
    jsPath: String,
    connection: JsonRpcConnection,
  ): State =
    val st = mergedCfg.init.fromFile(jsPath)
    val host = Initialize(st, spec, connection)
    val result = new RunToCompletion(st, Some(host)).result
    assert(
      wjiOk(result) == Success(Bool(true)),
      s"test case never set globalThis.__wjiOk = true: $jsPath (got ${wjiOk(result)})",
    )
    result
