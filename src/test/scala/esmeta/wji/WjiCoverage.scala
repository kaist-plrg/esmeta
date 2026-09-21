package esmeta.wji

import esmeta.{WJI_COVERAGE_LOG_DIR, WJI_JS_API_TEST_DIR}
import esmeta.cfg.Node
import esmeta.interpreter.{Interpreter => EsInterpreter}
import esmeta.state.State
import esmeta.util.SystemUtils.*
import esmeta.wji.bridge.host.WasmHost
import esmeta.wji.spec.SpecFile
import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*
import scala.collection.mutable

/** how much of the WebAssembly JS API surface WJI compiles (see
  * `WjiTest.wjiFuncNames`) the *official* conformance corpus
  * (`tests/wji/js-api/generated`, mirroring `spectec/test/js-api` --
  * `EvalSpec`'s own `wjiEvalTest` runs the exact same files) actually
  * exercises. Node (IR basic-block) granularity, not true spec-step
  * granularity -- WJI's compiled IR carries no `esmeta.util.Loc` at all (only
  * mainline ECMA-262's does), so a spec step and a WJI `Node` aren't
  * necessarily 1:1, but a `Node` is still a small, real slice of an
  * algorithm's control flow, and is the finest unit `esmeta.cfg` offers for
  * free (`Func.nodes = entry.reachable`, `cfg.funcOf` for the reverse
  * lookup) -- see the design discussion this followed for why full step-level
  * coverage would need new location-tracking plumbing through WJI's own
  * parser and every lowering pass instead.
  *
  * Not a `Suite` (this measures, it doesn't assert pass/fail) -- run directly:
  * {{{
  *   sbt "Test/runMain esmeta.wji.WjiCoverage"
  * }}}
  *
  * Every file under `tests/wji/js-api/generated` is attempted, including ones
  * `EvalSpec` itself skips or cancels (`skippedEntirely`/`slowFiles`) -- a
  * partial run still contributes whatever coverage it reaches before crashing
  * or timing out, and CI-friendliness (the reason those exist in `EvalSpec`)
  * doesn't apply to an occasional, manually-run measurement. One shared
  * timeout for every file rather than `EvalSpec`'s slow/default split, for the
  * same reason.
  */
object WjiCoverage:

  private val perFileTimeoutSec = 600

  /** mirrors `EsInterpreter`'s own `eval(node)` override point exactly the way
    * `esmeta.es.util.Coverage.Interp` does for ECMA-262 -- kept as a separate
    * small subclass here (rather than reusing `Coverage.Interp`) for the same
    * reason `WjiTest.RunToCompletion` is its own class rather than reusing
    * `ESTest.CheckAfter`: `esmeta.es` must not depend on `esmeta.wji`, but
    * `Coverage.Interp`'s hardcoded `extends Interpreter(initSt, tyCheck,
    * timeLimit)` never threads a `wasmHost` through, so a WJI test case
    * couldn't resolve any wasm host call under it anyway.
    */
  private class CoverageInterp(
    st: State,
    wasmHost: Option[WasmHost],
    timeLimit: Option[Int],
  ) extends EsInterpreter(st, wasmHost = wasmHost, timeLimit = timeLimit):
    val touchedNodes: mutable.Set[Node] = mutable.Set()
    override def eval(node: Node): Unit =
      touchedNodes += node
      super.eval(node)

  /** normalized (`NormalizeAlgoNamePass.normalize`'s own rule -- space to
    * underscore, lower-cased -- reproduced here rather than reached into,
    * since it's a private one-liner) base names of every algorithm WJI pulls
    * in from `webidl/index.bs` (`SpecFile.webidlFilter`) rather than from
    * `js-api/index.bs` itself. Excluded from the coverage report below: WJI
    * mechanizes almost none of these as real, invoked algorithms -- their
    * actual runtime effect is hardcoded directly by lowering passes
    * (`AddBuiltinBehaviourPass`/`AddInterfaceMemberBuiltinBehaviourPass`/...)
    * instead, so whether one of these compiled-but-uncalled functions is
    * "covered" says nothing about the WebAssembly JS API surface this tool
    * exists to measure -- counting them in the denominator would just dilute
    * that signal.
    */
  private val webidlBaseNames: Set[String] =
    SpecFile.webidlFilter.map(_.replace(' ', '_').toLowerCase)

  /** a lowering pass (`ExpandFollowingStepsPass`) can split a webidl
    * algorithm's body into extra `<name>_closureN` helper functions -- those
    * inherit their parent's webidl-ness by name prefix, since nothing tracks
    * per-`Algorithm` provenance through the lowering pipeline today (adding
    * that would be its own, separate effort, on the order of the `Loc`
    * propagation the line-coverage design discussion decided against for
    * now).
    */
  private def isWebIdlDerived(funcName: String): Boolean =
    webidlBaseNames.exists(base =>
      funcName == base || funcName.startsWith(s"${base}_closure"),
    )

  private case class FuncCoverage(name: String, touched: Int, total: Int)
  private given Encoder[FuncCoverage] = deriveEncoder

  private case class CoverageReport(
    totalFuncs: Int,
    touchedFuncs: Int,
    totalNodes: Int,
    touchedNodes: Int,
    perFunc: List[FuncCoverage],
  )
  private given Encoder[CoverageReport] = deriveEncoder

  def main(args: Array[String]): Unit =
    val cfg = WjiTest.mergedCfg
    var connection = Initialize.startProcess()
    val touchedNodes: mutable.Set[Node] = mutable.Set()

    val files = walkTree(WJI_JS_API_TEST_DIR).filter(f => jsFilter(f.getName))
    for (file, i) <- files.zipWithIndex do
      print(s"[${i + 1}/${files.size}] ${file.getName} ... ")
      try
        val st = cfg.init.fromFile(file.toString)
        val host = Initialize(st, WjiTest.spec, connection)
        val interp = new CoverageInterp(st, Some(host), Some(perFileTimeoutSec))
        interp.result
        touchedNodes ++= interp.touchedNodes
        println(s"done (${interp.touchedNodes.size} nodes touched)")
      catch
        case e: Throwable =>
          val poisoned = connection.isPoisoned
          if poisoned then
            connection.close()
            connection = Initialize.startProcess()
          println(s"FAILED (poisoned=$poisoned) -- ${e.getClass.getSimpleName}: ${e.getMessage}")
    connection.close()

    val perFunc = cfg.funcs
      .filter(f => WjiTest.wjiFuncNames(f.name) && !isWebIdlDerived(f.name))
      .map(f => FuncCoverage(f.name, f.nodes.count(touchedNodes), f.nodes.size))
      .sortBy(f => (f.total match { case 0 => 1.0; case t => f.touched.toDouble / t }, f.name))

    val report = CoverageReport(
      totalFuncs = perFunc.size,
      touchedFuncs = perFunc.count(_.touched > 0),
      totalNodes = perFunc.map(_.total).sum,
      touchedNodes = perFunc.map(_.touched).sum,
      perFunc = perFunc,
    )

    println()
    println(s"=== WJI node coverage (${report.touchedFuncs}/${report.totalFuncs} funcs, " +
      s"${report.touchedNodes}/${report.totalNodes} nodes touched) ===")
    for f <- perFunc do
      val pct = if f.total == 0 then 0.0 else f.touched * 100.0 / f.total
      println(f"$pct%6.1f%%  ${f.touched}%4d/${f.total}%-4d  ${f.name}")

    mkdir(WJI_COVERAGE_LOG_DIR)
    dumpJson("WJI coverage report", report, s"$WJI_COVERAGE_LOG_DIR/coverage.json")
    dumpFile(
      "zero-coverage WJI functions",
      perFunc.filter(_.touched == 0).map(_.name).mkString("\n"),
      s"$WJI_COVERAGE_LOG_DIR/zero-coverage-funcs",
    )
