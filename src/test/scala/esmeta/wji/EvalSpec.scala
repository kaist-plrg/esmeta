package esmeta.wji

import esmeta.{BASE_DIR, WJI_JS_API_TEST_DIR, WJI_MANUAL_TEST_DIR}
import esmeta.es.ESTest.checkExit
import esmeta.util.SystemUtils.*
import esmeta.wji.bridge.rpc.JsonRpcConnection
import java.nio.file.Paths
import org.scalatest.{Args, BeforeAndAfterAll, Status, Tag}
import org.scalatest.funsuite.AnyFunSuite
import scala.util.Success

/** tags every test in [[EvalSpec]] so `basicTest` can exclude them with `-l
  * esmeta.wji.EvalTag` while `wjiEvalTest` still runs them directly by class
  * name — see `build.sbt`.
  */
object EvalTag extends Tag("esmeta.wji.EvalTag")

/** test cases decided out of scope entirely (see each entry's own comment) —
  * skipped without even running the WJI interpreter, unlike
  * [[expectedFailingSubtests]] below (whose files *do* run for real). Names are
  * cancelled rather than run, so `wjiEvalTest` stays green while these stay
  * excluded — remove a name here only if the underlying scope decision is ever
  * revisited. Re-reproduce by pasting a name below verbatim into `sbt run
  * wji-eval <name> -silent` (from the repo root) if picking one back up — each
  * name below already *is* the real, repo-root-relative path to the file
  * (`BASE_DIR.relativize`, see the `for` loop below), not some other, unrelated
  * shorthand that would need translating by hand first. Keyed by full path
  * rather than bare filename — js-api's generated fixtures mirror
  * spectec/test/js-api's own directory structure, which reuses the same
  * filename (e.g. `toString.any.js`) across multiple categories.
  */
private val skippedEntirely: Set[String] =
  Set(
    // Implementation-defined Limits section (locals/params/etc. count caps)
    // is a purely declarative constraint list no algorithm ever references —
    // decided out of scope entirely, doesn't fit ESMeta/SpecTec's
    // algorithm-execution model (docs/out_of_scope.md #7, #8).
    "tests/wji/js-api/generated/limits.any.js",
    // Module.prototype.customSections needs a real wasm binary-format parser
    // (section id + LEB128 varint length) — a new component WJI doesn't have,
    // not a small lowering-pass fix (docs/out_of_scope.md #1).
    "tests/wji/js-api/generated/module/customSections.any.js",
  )

/** test cases that DO run for real (unlike [[skippedEntirely]] above), but are
  * known to always fail one or more specific subtests for a settled, standing
  * reason rather than an in-progress WJI gap — mapped to exactly the
  * `"|||"`-joined subtest-name string [[WjiTest.failingSubtests]] is expected
  * to return. Checked with a real assertion instead of being skipped outright,
  * so a change in exactly what fails (a regression elsewhere in the file, or
  * the known deviation finally getting patched to match real engines) surfaces
  * as a loud, actionable test failure rather than silently staying green (or
  * silently staying cancelled) either way.
  */
private val expectedFailingSubtests: Map[String, String] =
  Map(
    // "Setting non-function": table.set(0, undefined)/.grow(1, undefined)
    // expect a real ToWebAssemblyValue conversion (TypeError), but WJI
    // faithfully compiles the spec's "value is missing" check, which treats
    // an explicit undefined the same as omission — real engines deviate from
    // the spec text here, so this is intentionally left spec-faithful rather
    // than patched to match them (docs/engine_deviations.md #1).
    "tests/wji/js-api/generated/table/get-set.any.js" -> "Setting non-function",
  )

/** test cases that are correct but too slow to run on every `wjiEvalTest` —
  * unlike [[skippedEntirely]] (a settled scope exclusion), these just take a
  * while (275-290s for the corpus's two largest files, 55/208 subtests,
  * `TODO.md` #58's `WebAssembly.instantiate` overload-dispatch fix,
  * `personal/DONE.md` #63; ~330s for `js-string/basic.any.js`, `TODO.md` #56's
  * own resolution — every one of its 6 `test()` blocks exercises all 13
  * builtins against a large combinatorial input set) once actually run to
  * completion rather than crashing early. Cancelled by default, same as
  * [[skippedEntirely]], unless `-Dslow=true` is passed — see [[EvalSpec.slow]].
  */
private val slowFiles: Set[String] =
  Set(
    "tests/wji/js-api/generated/constructor/instantiate.any.js",
    "tests/wji/js-api/generated/constructor/instantiate-bad-imports.any.js",
    "tests/wji/js-api/generated/js-string/basic.any.js",
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
  * green run doesn't drown in a wall of prints), same as running [[slowFiles]]
  * at all — `wjiEvalTest` itself is a fixed alias with no room for extra args,
  * so either needs `testOnly` directly, same as [[SnapshotSpec]]'s
  * `-Dupdate=true`:
  * {{{
  *   sbt "testOnly esmeta.wji.EvalSpec -- -Dverbose=true -Dslow=true"
  * }}}
  */
class EvalSpec extends AnyFunSuite with BeforeAndAfterAll:

  private var verbose = false
  private var slow = false
  override def run(testName: Option[String], args: Args): Status =
    verbose = args.configMap.getWithDefault("verbose", "false") == "true"
    slow = args.configMap.getWithDefault("slow", "false") == "true"
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
    * comfortably above every legitimately-slow test observed so far, including
    * `memory/grow.any.js`'s own ~80-90s and js-api's `limits.any.js`'s own ~80s
    * (both under a fresh, cold-started connection; a warm/shared one should
    * only be faster). Throws `TimeoutException` (unrelated to SpecTec, so
    * `connection.isPoisoned` correctly stays false and no respawn is needed)
    * rather than needing an external process kill.
    *
    * Not sized around [[slowFiles]] (each ~275-290s) -- those are cancelled by
    * default rather than actually run, so they don't need to fit here; a
    * `-Dslow=true` run passes its own longer [[slowFileTimeoutSec]] instead.
    *
    * No longer sized around the risk of `limits.any.js` (spec-mandated stress
    * test building up to 10M wasm constructs -- the corpus's one file marked
    * `// META: timeout=long` even for real engines) running for the rest of the
    * suite's lifetime: its genuinely-unbounded calls are now neutered at
    * generation time (`tests/wji/scripts/wji-generate-js-api-tests.js`'s
    * `perFilePatches`, see `docs/out_of_scope.md` #3), leaving only the ones
    * cheap enough to actually finish -- this constant just needs to cover that
    * reduced, now-finite worst case, same as everything else here.
    */
  private val perTestTimeoutSec = 150

  /** [[perTestTimeoutSec]]'s own counterpart for a [[slowFiles]] entry, used
    * only on a `-Dslow=true` run -- comfortably above the ~275-290s each
    * actually took standalone (`sbt run wji-eval ... -silent`).
    */
  private val slowFileTimeoutSec = 500

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
      if skippedEntirely(name) then cancel("decided out of scope")
      else if slowFiles(name) && !slow then
        cancel("slow test, opt-in via -Dslow=true")
      else
        val timeoutSec =
          if slowFiles(name) then slowFileTimeoutSec else perTestTimeoutSec
        try
          expectedFailingSubtests.get(name) match
            case Some(expected) =>
              val result =
                WjiTest.runFile(file.toString, connection, Some(timeoutSec))
              checkExit(result)
              val actual = WjiTest.failingSubtests(result)
              assert(
                actual == Success(expected),
                s"expected exactly {$expected} to fail in $name, but got " +
                s"${actual} -- either a new regression or the known " +
                s"deviation was fixed; update expectedFailingSubtests either way",
              )
            case None =>
              checkExit(
                WjiTest.evalFile(file.toString, connection, Some(timeoutSec)),
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
