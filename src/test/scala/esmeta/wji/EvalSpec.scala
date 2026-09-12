package esmeta.wji

import esmeta.{BASE_DIR, WJI_JS_API_TEST_DIR, WJI_MANUAL_TEST_DIR}
import esmeta.es.ESTest.checkExit
import esmeta.util.SystemUtils.*
import esmeta.wji.bridge.rpc.JsonRpcConnection
import java.nio.file.Paths
import org.scalatest.{Args, BeforeAndAfterAll, Status, Tag}
import org.scalatest.funsuite.AnyFunSuite

/** tags every test in [[EvalSpec]] so `basicTest` can exclude them with `-l
  * esmeta.wji.EvalTag` while `wjiEvalTest` still runs them directly by class
  * name — see `build.sbt`.
  */
object EvalTag extends Tag("esmeta.wji.EvalTag")

/** test cases that are known to hit an unmechanized gap rather than a bug in
  * the test case itself. Cancelled rather than run, so `wjiEvalTest` stays
  * green while the gap is worked on — remove a test case's name here once it's
  * fixed. No per-test-case reason kept here — it shifts with every partial fix,
  * so keeping it in sync would be pure churn; re-reproduce by pasting a name
  * below verbatim into `sbt run wji-eval <name> -silent` (from the repo root)
  * when picking one back up — each name below already *is* the real,
  * repo-root-relative path to the file (`BASE_DIR.relativize`, see the `for`
  * loop below), not some other, unrelated shorthand that would need translating
  * by hand first. Keyed by full path rather than bare filename — js-api's
  * generated fixtures mirror spectec/test/js-api's own directory structure,
  * which reuses the same filename (e.g. `toString.any.js`) across multiple
  * categories.
  */
private val knownFailing: Set[String] =
  Set(
    // js-api/generated: `tests/wji/js-api/dataview-polyfill.js` works around
    // ESMeta not mechanizing DataView, the mainline CondParser fix for "X is
    // TYPE that has a [[SLOT]] internal slot" (docs/esmeta_errors.md #3)
    // unblocked TypedArray.prototype.set, WebIdlConversion learned TagType +
    // ExceptionOptions + GlobalDescriptor.mutable's IDL default + sequence<T>
    // conversion (docs/hardcodes.md #1/#2), an omitted optional dictionary
    // argument now actually gets converted (AddInterfaceMemberBuiltinBehaviourPass.
    // omittedBranch), and Instr.ForEachPaired handles "X and Y of A and B,
    // paired linearly", and a manual rule (`manuals/rule.json`, mainline
    // `esmeta.compiler.Compiler`) now maps Math.floor's own defining prose
    // ("the greatest (closest to +∞) integral Number value that is not
    // greater than X") to the existing `floor` unary op, since it's a
    // singleton phrasing (ecma262 never states it any other way) rather than
    // a recurring idiom worth a real grammar rule -- so these now fail on
    // the *next* gap each hits: a required WebIDL member missing (still no
    // way to throw a real `TypeError` for it, e.g. TableDescriptor.element),
    // an accessor property descriptor read via `.Value` instead of invoking
    // its getter (dictionary reads assume data properties only), missing
    // branding checks (`not a proper reference base: undefined`), the
    // still-unmechanized SharedArrayBuffer/IEEE754-rounding-phrasing/etc.,
    // and `limits.any.js` (a spec-mandated stress test building up to 10M
    // wasm constructs -- marked `// META: timeout=long` even for real
    // engines, so it's just too slow for WJI's interpreter rather than
    // blocked by a real gap).
    "tests/wji/js-api/generated/constructor/compile.any.js",
    "tests/wji/js-api/generated/constructor/instantiate-bad-imports.any.js",
    "tests/wji/js-api/generated/constructor/instantiate.any.js",
    "tests/wji/js-api/generated/constructor/multi-value.any.js",
    "tests/wji/js-api/generated/constructor/validate.any.js",
    "tests/wji/js-api/generated/exception/jsTag.tentative.any.js",
    "tests/wji/js-api/generated/global/constructor.any.js",
    "tests/wji/js-api/generated/global/value-get-set.any.js",
    "tests/wji/js-api/generated/instance/constructor-bad-imports.any.js",
    "tests/wji/js-api/generated/instance/constructor.any.js",
    "tests/wji/js-api/generated/interface.any.js",
    "tests/wji/js-api/generated/js-string/basic.any.js",
    "tests/wji/js-api/generated/js-string/constants.any.js",
    "tests/wji/js-api/generated/js-string/imports.any.js",
    "tests/wji/js-api/generated/limits.any.js",
    "tests/wji/js-api/generated/memory/constructor.any.js",
    "tests/wji/js-api/generated/memory/grow.any.js",
    "tests/wji/js-api/generated/module/constructor.any.js",
    "tests/wji/js-api/generated/module/customSections.any.js",
    "tests/wji/js-api/generated/module/imports.any.js",
    "tests/wji/js-api/generated/table/constructor.any.js",
    "tests/wji/js-api/generated/table/get-set.any.js",
    "tests/wji/js-api/generated/table/grow-memory64.any.js",
  )

/** Runs every `.js` test case under `tests/wji/manual` and
  * `tests/wji/js-api/generated` end to end through the merged WJI IR program
  * (see [[WjiTest]]). Each test case is standalone and self-checking: it must
  * set `globalThis.__wjiOk = true` itself once every check it performs (sync
  * `throw`, async or otherwise) has passed — see `tests/wji/manual/README.md`
  * and [[WjiTest]] for why a bare `throw` alone isn't enough for checks made
  * inside a `.then()` callback. `tests/wji/js-api/generated`'s fixtures set it
  * via `report-shim.js`, aggregating every WPT-style subtest in the file into
  * one boolean (pass iff every subtest passed) — see
  * `tests/wji/js-api/README.md`.
  *
  * Not part of the default `sbt test`/`basicTest` tier — this suite spawns a
  * real external SpecTec process (shared across its test cases, see
  * `connection` below), so this is its own opt-in task:
  * {{{
  *   sbt --client wjiEvalTest
  * }}}
  *
  * Per-test timing + failure cause are opt-in (silent by default, so a normal
  * green run doesn't drown in a wall of prints) — `wjiEvalTest` itself is a
  * fixed alias with no room for extra args, so this needs `testOnly` directly,
  * same as [[SnapshotSpec]]'s `-Dupdate=true`:
  * {{{
  *   sbt "testOnly esmeta.wji.EvalSpec -- -Dverbose=true"
  * }}}
  */
class EvalSpec extends AnyFunSuite with BeforeAndAfterAll:

  private var verbose = false
  override def run(testName: Option[String], args: Args): Status =
    verbose = args.configMap.getWithDefault("verbose", "false") == "true"
    super.run(testName, args)

  /** the one SpecTec process/connection shared across every test case in this
    * suite (see `Initialize.startProcess`'s doc for why: process spawn + spec
    * parse is ~10s, dwarfing a test's own ~1-2s). Reassigned, not just closed,
    * on a per-test exception, but only when `connection.isPoisoned` — i.e. only
    * when the exception actually escaped mid-RPC-turn (a `HostFunction` bug
    * thrown while SpecTec was blocked waiting on a `host_func_invoke` reply,
    * see [[JsonRpcConnection.isPoisoned]]/`serve`), which leaves no response
    * line written for that inbound request and wedges the connection for every
    * line written to it afterward. Most test failures (a plain interpreter
    * error, or a subtest assertion that just didn't hold) never touch `serve`
    * at all, so the connection is still perfectly healthy and reusable — paying
    * the ~10s respawn for those would be pure waste. Bounding a genuinely
    * wedged test's blast radius to itself (plus one extra ~10s process spawn
    * for the next test) still matches the isolation a fresh process per test
    * gave for free.
    */
  private var connection: JsonRpcConnection = _

  override def beforeAll(): Unit = connection = Initialize.startProcess()
  override def afterAll(): Unit = connection.close()

  /** bounds a single test case's wall-clock time (checked periodically by
    * `esmeta.interpreter.Interpreter` itself, see `timeLimit` there) --
    * comfortably above every legitimately-slow test observed so far (worst case
    * ~35s, wasm-module-builder-heavy files under a warm/shared connection), but
    * well short of a file like js-api's `limits.any.js` (spec-mandated stress
    * test building up to 10M wasm constructs -- marked `// META: timeout=long`
    * even for real engines) that would otherwise run for the rest of the
    * suite's lifetime. Throws `TimeoutException` (unrelated to SpecTec, so
    * `connection.isPoisoned` correctly stays false and no respawn is needed)
    * rather than needing an external process kill.
    */
  private val perTestTimeoutSec = 60

  private val roots: List[String] =
    List(WJI_MANUAL_TEST_DIR, WJI_JS_API_TEST_DIR)

  for
    dir <- roots
    file <- walkTree(dir) if jsFilter(file.getName)
  do
    // repo-root-relative, so it doubles as a real path -- `dir` itself is
    // already `$BASE_DIR/tests/wji/...`, no separate per-root label needed.
    val name = Paths.get(BASE_DIR).relativize(file.toPath).toString
    test(name, EvalTag) {
      val start = System.nanoTime()
      def elapsed = (System.nanoTime() - start) / 1e9
      if knownFailing(name) then cancel("known WJI mechanization gap")
      else
        try
          checkExit(
            WjiTest.evalFile(
              file.toString,
              connection,
              Some(perTestTimeoutSec),
            ),
          )
          if verbose then println(f"[$elapsed%.1fs] $name")
        catch
          case e: Throwable =>
            val poisoned = connection.isPoisoned
            if poisoned then
              connection.close()
              connection = Initialize.startProcess()
            if verbose then
              println(
                f"[$elapsed%.1fs] $name FAILED (poisoned=$poisoned) -- ${e.getClass.getSimpleName}: ${e.getMessage}",
              )
            throw e
    }
