package esmeta.wji

import esmeta.WJI_TEST_DIR
import esmeta.es.ESTest.checkExit
import esmeta.ir.NormalInsts
import esmeta.util.SystemUtils.*
import org.scalatest.Tag
import org.scalatest.funsuite.AnyFunSuite

/** tags every test in [[EvalSpec]] so `basicTest` can exclude them with `-l
  * esmeta.wji.EvalTag` while `wjiEvalTest` still runs them directly by class
  * name — see `build.sbt`.
  */
object EvalTag extends Tag("esmeta.wji.EvalTag")

/** Runs every `.js` fixture under `tests/wji` end to end through the merged WJI
  * IR program (see [[WjiTest]]). An optional sibling `.ir` file supplies extra
  * `assert` instructions checked against the final `State`; fixtures without
  * one are treated as smoke tests (must simply run to completion without an
  * uncaught exception).
  *
  * Not part of the default `sbt test`/`basicTest` tier — each fixture spawns a
  * real external SpecTec process, so this is its own opt-in task:
  * {{{
  *   sbt --client wjiEvalTest
  * }}}
  */
class EvalSpec extends AnyFunSuite:
  for file <- walkTree(WJI_TEST_DIR) if jsFilter(file.getName) do
    test(file.getName, EvalTag) {
      val jsName = file.toString
      val irName = changeExt("js", "ir")(jsName)
      val checkAfter =
        if exists(irName) then NormalInsts.fromFile(irName) else Nil
      checkExit(WjiTest.evalFile(jsName, checkAfter))
    }
