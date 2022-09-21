package esmeta.es.util.fuzzer

import esmeta.es.*
import esmeta.es.util.*
import esmeta.util.BaseUtils.*

/** mutation target selector for fuzzer */
trait TargetSelector {
  def apply(
    pool: Iterable[Script],
    cov: Coverage,
    debug: Boolean = false,
  ): Script
}

/** weighted mutation target selector */
class WeightedSelector(pairs: (TargetSelector, Int)*) extends TargetSelector {
  def apply(
    pool: Iterable[Script],
    cov: Coverage,
    debug: Boolean = false,
  ): Script = weightedChoose(pairs)(pool, cov, debug)
}

/** branch coverage-based mutation target selector */
object BranchSelector extends TargetSelector {
  def apply(
    pool: Iterable[Script],
    cov: Coverage,
    debug: Boolean = false,
  ): Script =
    val cond = choose(cov.targetConds)
    if (debug) println(s"[BranchSelector] $cond")
    cov
      .getScript(cond)
      .getOrElse({
        if (debug) warn("BUT FAIL -> Use RandomSelector")
        RandomSelector(pool, cov, debug)
      })
}

/** random mutation target selector */
object RandomSelector extends TargetSelector {
  def apply(
    pool: Iterable[Script],
    cov: Coverage,
    debug: Boolean = false,
  ): Script =
    if (debug) println(s"[RandomSelector]")
    choose(pool)
}
