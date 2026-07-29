package esmeta.wji

import esmeta.WJI_TEST_DIR
import esmeta.es.ESTest.checkExit
import esmeta.util.SystemUtils.*
import org.scalatest.Tag
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
  * wji-eval tests/wji/<name>.js -silent` when picking one back up.
  */
private val knownFailing: Set[String] =
  Set.empty

/** Runs every `.js` test case under `tests/wji` end to end through the merged
  * WJI IR program (see [[WjiTest]]). Each test case is standalone and
  * self-checking: it must set `globalThis.__wjiOk = true` itself once every
  * check it performs (sync `throw`, async or otherwise) has passed — see
  * `tests/wji/README.md` and [[WjiTest]] for why a bare `throw` alone isn't
  * enough for checks made inside a `.then()` callback.
  *
  * Not part of the default `sbt test`/`basicTest` tier — each test case spawns
  * a real external SpecTec process, so this is its own opt-in task:
  * {{{
  *   sbt --client wjiEvalTest
  * }}}
  */
class EvalSpec extends AnyFunSuite:
  for file <- walkTree(WJI_TEST_DIR) if jsFilter(file.getName) do
    val name = file.getName
    test(name, EvalTag) {
      if knownFailing(name) then cancel("known WJI mechanization gap")
      else checkExit(WjiTest.evalFile(file.toString))
    }
