package esmeta.wji

import esmeta.WJI_MANUAL_TEST_DIR
import esmeta.es.ESTest.checkExit
import esmeta.util.SystemUtils.*
import esmeta.wji.bridge.rpc.JsonRpcConnection
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
  * wji-eval tests/wji/manual/<name>.js -silent` when picking one back up.
  */
private val knownFailing: Set[String] =
  Set()

/** Runs every `.js` test case under `tests/wji/manual` end to end through the
  * merged WJI IR program (see [[WjiTest]]). Each test case is standalone and
  * self-checking: it must set `globalThis.__wjiOk = true` itself once every
  * check it performs (sync `throw`, async or otherwise) has passed — see
  * `tests/wji/manual/README.md` and [[WjiTest]] for why a bare `throw` alone
  * isn't enough for checks made inside a `.then()` callback.
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

  for file <- walkTree(WJI_MANUAL_TEST_DIR) if jsFilter(file.getName) do
    val name = file.getName
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
