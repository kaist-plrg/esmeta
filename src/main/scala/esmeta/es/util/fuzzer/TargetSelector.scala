package esmeta.es.util.fuzzer

import esmeta.spec.Grammar
import esmeta.es.*
import esmeta.es.util.*
import esmeta.util.BaseUtils.*

/** mutation target selector for fuzzer */
trait TargetSelector {
  def apply(pool: Iterable[Script], cov: Coverage): (String, Script)
  def apply(
    pool: Iterable[Script],
    cov: Coverage,
    grammar: Grammar,
    debug: Boolean,
  ): Script =
    val (name, script) = apply(pool, cov)
    if (debug)
      val scriptStr = script.toString(grammar)
      println(f"[$name%-20s] $scriptStr")
    script
}

/** weighted mutation target selector */
class WeightedSelector(pairs: (TargetSelector, Int)*) extends TargetSelector {
  def apply(pool: Iterable[Script], cov: Coverage): (String, Script) =
    weightedChoose(pairs)(pool, cov)
}

/** branch coverage-based mutation target selector */
object BranchSelector extends TargetSelector {
  def apply(pool: Iterable[Script], cov: Coverage): (String, Script) =
    val cond = choose(cov.targetConds)
    cov.getScript(cond).fold(RandomSelector(pool, cov)) {
      (s"Branch - $cond", _)
    }
}

/** random mutation target selector */
object RandomSelector extends TargetSelector {
  def apply(pool: Iterable[Script], cov: Coverage): (String, Script) =
    ("Random", choose(pool))
}
