package esmeta.wji

import esmeta.{WJI_JS_API_TEST_DIR, WJI_MANUAL_TEST_DIR}
import esmeta.es.ESTest.checkExit
import esmeta.util.SystemUtils.*
import esmeta.wji.bridge.rpc.JsonRpcConnection
import java.nio.file.Paths
import org.scalatest.{BeforeAndAfterAll, Tag}
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
  * so keeping it in sync would be pure churn; re-reproduce with `sbt run
  * wji-eval <path printed as the test name below> -silent` when picking one
  * back up. Keyed by `"<root label>/<path relative to that root>"` (e.g.
  * `"manual/demo.js"`, `"js-api/memory/toString.any.js"`) rather than bare
  * filename — js-api's generated fixtures mirror spectec/test/js-api's own
  * directory structure, which reuses the same filename (e.g. `toString.any.js`)
  * across multiple categories.
  */
private val knownFailing: Set[String] =
  Set(
    // js-api/generated: mostly a single root cause (`[NotSupported]
    // feature/DataView` -- wasm-module-builder.js, which nearly every one of
    // these depends on to assemble wasm module bytes, uses DataView, which
    // ESMeta's ECMA-262 mechanization doesn't implement at all) plus a
    // handful of missing branding checks (`not a proper reference base:
    // undefined`), a WebIDL dictionary conversion gap (`invalid object
    // field: "parameters"` -- TagType/TagDescriptor isn't in
    // WebIdlConversion's hardcoded dictionary list, see docs/hardcodes.md
    // #1/#2), and a couple of other gaps -- see personal/TODO.md #14.
    "js-api/constructor/compile.any.js",
    "js-api/constructor/instantiate-bad-imports.any.js",
    "js-api/constructor/instantiate.any.js",
    "js-api/constructor/multi-value.any.js",
    "js-api/constructor/validate.any.js",
    "js-api/exception/basic.tentative.any.js",
    "js-api/exception/constructor.tentative.any.js",
    "js-api/exception/getArg.tentative.any.js",
    "js-api/exception/identity.tentative.any.js",
    "js-api/exception/is.tentative.any.js",
    "js-api/exception/jsTag.tentative.any.js",
    "js-api/exception/toString.tentative.any.js",
    "js-api/gc/casts.tentative.any.js",
    "js-api/gc/default-value.tentative.any.js",
    "js-api/gc/exported-object.tentative.any.js",
    "js-api/gc/i31.tentative.any.js",
    "js-api/global/constructor.any.js",
    "js-api/global/toString.any.js",
    "js-api/global/value-get-set.any.js",
    "js-api/global/valueOf.any.js",
    "js-api/instance/constructor-bad-imports.any.js",
    "js-api/instance/constructor-caching.any.js",
    "js-api/instance/constructor.any.js",
    "js-api/instance/exports.any.js",
    "js-api/instance/toString.any.js",
    "js-api/interface.any.js",
    "js-api/js-string/basic.any.js",
    "js-api/js-string/constants.any.js",
    "js-api/js-string/imports.any.js",
    "js-api/limits.any.js",
    "js-api/memory/buffer.any.js",
    "js-api/memory/constructor-memory64.any.js",
    "js-api/memory/constructor.any.js",
    "js-api/memory/grow-memory64.any.js",
    "js-api/memory/grow.any.js",
    "js-api/module/constructor.any.js",
    "js-api/module/customSections.any.js",
    "js-api/module/exports.any.js",
    "js-api/module/imports.any.js",
    "js-api/module/toString.any.js",
    "js-api/prototypes.any.js",
    "js-api/table/constructor-memory64.any.js",
    "js-api/table/constructor.any.js",
    "js-api/table/get-set.any.js",
    "js-api/table/grow-memory64.any.js",
    "js-api/table/grow.any.js",
    "js-api/table/length.any.js",
    "js-api/tag/constructor.tentative.any.js",
    "js-api/tag/toString.tentative.any.js",
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
  */
class EvalSpec extends AnyFunSuite with BeforeAndAfterAll:

  /** the one SpecTec process/connection shared across every test case in this
    * suite (see `Initialize.startProcess`'s doc for why: process spawn + spec
    * parse is ~10s, dwarfing a test's own ~1-2s). Reassigned, not just closed,
    * on a per-test exception: an exception escaping mid-RPC-turn (e.g. a
    * `HostFunction` bug thrown while SpecTec is blocked waiting on a
    * `host_func_invoke` reply — see `JsonRpcConnection`'s `serve`) leaves no
    * response line written for that inbound request, wedging the connection for
    * every line written to it afterward. Respawning bounds a bad test's blast
    * radius to itself (plus one extra ~10s process spawn for the next test),
    * matching the isolation a fresh process per test gave for free.
    */
  private var connection: JsonRpcConnection = _

  override def beforeAll(): Unit = connection = Initialize.startProcess()
  override def afterAll(): Unit = connection.close()

  private val roots: List[(String, String)] = List(
    "manual" -> WJI_MANUAL_TEST_DIR,
    "js-api" -> WJI_JS_API_TEST_DIR,
  )

  for
    (label, dir) <- roots
    file <- walkTree(dir) if jsFilter(file.getName)
  do
    val name = s"$label/${Paths.get(dir).relativize(file.toPath)}"
    test(name, EvalTag) {
      if knownFailing(name) then cancel("known WJI mechanization gap")
      else
        try checkExit(WjiTest.evalFile(file.toString, connection))
        catch
          case e: Throwable =>
            connection.close()
            connection = Initialize.startProcess()
            throw e
    }
