package esmeta.wji

import esmeta.{WJI_COVERAGE_LOG_DIR, WJI_JS_API_TEST_DIR}
import esmeta.cfg.{Block, Branch, BranchKind, Call, Func, Node}
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
  * `EvalSpec`'s own `wjiEvalTest` runs the exact same files) actually exercises
  * -- both node (IR basic-block) and branch (which side of a `Branch` node's
  * outcome got taken) coverage. Node granularity, not true spec-step
  * granularity -- WJI's compiled IR carries no `esmeta.util.Loc` at all (only
  * mainline ECMA-262's does), so a spec step and a WJI `Node` aren't
  * necessarily 1:1, but a `Node` is still a small, real slice of an algorithm's
  * control flow, and is the finest unit `esmeta.cfg` offers for free
  * (`Func.nodes = entry.reachable`, `cfg.funcOf` for the reverse lookup) -- see
  * the design discussion this followed for why full step-level coverage would
  * need new location-tracking plumbing through WJI's own parser and every
  * lowering pass instead.
  *
  * Besides the console summary and `coverage.json`/`zero-coverage-funcs` (see
  * [[main]]), also dumps one annotated IR file per function under
  * `logs/wji-coverage/annotated-ir/` (see [[dumpAnnotatedIR]]) -- the same `id:
  * content -> nextId` line shape `esmeta.cfg.util.Stringifier` prints, with a
  * `[x]`/`[ ]` marker on each node and on each side of each branch, so coverage
  * can be read right next to the real compiled IR instead of only as a
  * name-and-percentage table.
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
  * doesn't apply to an occasional, manually-run measurement. One shared timeout
  * for every file rather than `EvalSpec`'s slow/default split, for the same
  * reason.
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
    val touchedConds: mutable.Set[Cond] = mutable.Set()
    override def eval(node: Node): Unit =
      touchedNodes += node
      super.eval(node)
    override def moveBranch(branch: Branch, cond: Boolean): Unit =
      touchedConds += Cond(branch.id, cond)
      super.moveBranch(branch, cond)

  /** a taken outcome of one `Branch` node -- keyed by the branch's own node id
    * (unique within a `CFG`, and a plain `Int` is simpler to key a `Set` by
    * than the `Branch` node itself) rather than holding the `Branch` directly.
    * Mirrors `esmeta.es.util.Coverage.Cond`'s own `(branch, cond)` shape.
    */
  private case class Cond(branchId: Int, taken: Boolean)

  /** normalized (`NormalizeAlgoNamePass.normalize`'s own rule -- space to
    * underscore, lower-cased -- reproduced here rather than reached into, since
    * it's a private one-liner) base names of every algorithm WJI pulls in from
    * `webidl/index.bs` (`SpecFile.webidlFilter`) rather than from
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
    * propagation the line-coverage design discussion decided against for now).
    */
  private def isWebIdlDerived(funcName: String): Boolean =
    webidlBaseNames.exists(base =>
      funcName == base || funcName.startsWith(s"${base}_closure"),
    )

  private case class FuncCoverage(
    name: String,
    nodesTouched: Int,
    nodesTotal: Int,
    branchesTouched: Int,
    branchesTotal: Int,
  )
  private given Encoder[FuncCoverage] = deriveEncoder

  private case class CoverageReport(
    totalFuncs: Int,
    touchedFuncs: Int,
    totalNodes: Int,
    touchedNodes: Int,
    totalBranches: Int,
    touchedBranches: Int,
    perFunc: List[FuncCoverage],
  )
  private given Encoder[CoverageReport] = deriveEncoder

  def main(args: Array[String]): Unit =
    val cfg = WjiTest.mergedCfg
    var connection = Initialize.startProcess()
    val touchedNodes: mutable.Set[Node] = mutable.Set()
    val touchedConds: mutable.Set[Cond] = mutable.Set()

    val files = walkTree(WJI_JS_API_TEST_DIR).filter(f => jsFilter(f.getName))
    for (file, i) <- files.zipWithIndex do
      print(s"[${i + 1}/${files.size}] ${file.getName} ... ")
      try
        val st = cfg.init.fromFile(file.toString)
        val host = Initialize(st, WjiTest.spec, connection)
        val interp = new CoverageInterp(st, Some(host), Some(perFileTimeoutSec))
        interp.result
        touchedNodes ++= interp.touchedNodes
        touchedConds ++= interp.touchedConds
        println(s"done (${interp.touchedNodes.size} nodes touched)")
      catch
        case e: Throwable =>
          val poisoned = connection.isPoisoned
          if poisoned then
            connection.close()
            connection = Initialize.startProcess()
          println(
            s"FAILED (poisoned=$poisoned) -- ${e.getClass.getSimpleName}: ${e.getMessage}",
          )
    connection.close()

    val perFunc = cfg.funcs
      .filter(f => WjiTest.wjiFuncNames(f.name) && !isWebIdlDerived(f.name))
      .map { f =>
        val branchIds = f.nodes.collect { case b: Branch => b.id }
        FuncCoverage(
          name = f.name,
          nodesTouched = f.nodes.count(touchedNodes),
          nodesTotal = f.nodes.size,
          branchesTouched = touchedConds.count(c => branchIds(c.branchId)),
          branchesTotal = branchIds.size * 2,
        )
      }
      .sortBy(f =>
        (
          f.nodesTotal match {
            case 0 => 1.0
            case t => f.nodesTouched.toDouble / t
          },
          f.name,
        ),
      )

    val report = CoverageReport(
      totalFuncs = perFunc.size,
      touchedFuncs = perFunc.count(_.nodesTouched > 0),
      totalNodes = perFunc.map(_.nodesTotal).sum,
      touchedNodes = perFunc.map(_.nodesTouched).sum,
      totalBranches = perFunc.map(_.branchesTotal).sum,
      touchedBranches = perFunc.map(_.branchesTouched).sum,
      perFunc = perFunc,
    )

    println()
    println(
      s"=== WJI coverage (${report.touchedFuncs}/${report.totalFuncs} funcs, " +
      s"${report.touchedNodes}/${report.totalNodes} nodes, " +
      s"${report.touchedBranches}/${report.totalBranches} branches) ===",
    )
    for f <- perFunc do
      val pct =
        if f.nodesTotal == 0 then 0.0 else f.nodesTouched * 100.0 / f.nodesTotal
      println(
        f"$pct%6.1f%%  ${f.nodesTouched}%4d/${f.nodesTotal}%-4d nodes  " +
        f"${f.branchesTouched}%3d/${f.branchesTotal}%-3d branches  ${f.name}",
      )

    mkdir(WJI_COVERAGE_LOG_DIR)
    dumpJson(
      "WJI coverage report",
      report,
      s"$WJI_COVERAGE_LOG_DIR/coverage.json",
    )
    dumpFile(
      "zero-coverage WJI functions",
      perFunc.filter(_.nodesTouched == 0).map(_.name).mkString("\n"),
      s"$WJI_COVERAGE_LOG_DIR/zero-coverage-funcs",
    )
    dumpAnnotatedIR(
      cfg.funcs.filter(f => perFunc.exists(_.name == f.name)),
      touchedNodes,
      touchedConds,
    )

  /** dumps one file per (non-webidl) WJI function, mirroring
    * `esmeta.cfg.util.Stringifier`'s own `Node`/`Branch` line format (`id:
    * content -> nextId`, `id: if/while cond then thenId else elseId`) but with
    * a `[x]`/`[ ]` coverage marker prefixed to each node, and to each of a
    * `Branch` node's two outcomes separately -- lets a reader see, at a glance,
    * exactly which basic blocks and which side of which conditional the
    * official corpus never reaches, next to the real compiled IR rather than a
    * name-only list. CFG-`Node` granularity, same caveat as the rest of this
    * tool -- see the class doc.
    */
  private def dumpAnnotatedIR(
    funcs: List[Func],
    touchedNodes: collection.Set[Node],
    touchedConds: collection.Set[Cond],
  ): Unit =
    val dir = s"$WJI_COVERAGE_LOG_DIR/annotated-ir"
    mkdir(dir, remove = true)
    def mark(b: Boolean): String = if b then "[x]" else "[ ]"
    def nodeLine(node: Node): String = node match
      case Block(id, insts, next) =>
        val body = insts.map(_.toString.trim).mkString("; ")
        val arrow = next.fold("")(n => s" -> ${n.id}")
        s"$id: ${mark(touchedNodes(node))} $body$arrow"
      case Call(id, callInst, next) =>
        val arrow = next.fold("")(n => s" -> ${n.id}")
        s"$id: ${mark(touchedNodes(node))} ${callInst.toString.trim}$arrow"
      case Branch(id, kind, cond, _, thenNode, elseNode) =>
        val kw = if kind == BranchKind.While then "while" else "if"
        val thenStr = thenNode.fold("<none>") { n =>
          s"${n.id}${mark(touchedConds(Cond(id, true)))}"
        }
        val elseStr = elseNode.fold("<none>") { n =>
          s"${n.id}${mark(touchedConds(Cond(id, false)))}"
        }
        s"$id: ${mark(touchedNodes(node))} $kw ${cond.toString.trim} then $thenStr else $elseStr"
    for func <- funcs do
      val lines = func.nodes.toList.sortBy(_.id).map(nodeLine)
      dumpFile(
        (List(s"${func.name}(${func.params.mkString(", ")})") ++ lines)
          .mkString("\n"),
        s"$dir/${func.name}.ir",
      )
    println(s"- Dumped annotated per-function IR into `$dir` .")
