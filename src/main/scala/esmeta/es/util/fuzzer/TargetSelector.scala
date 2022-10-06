package esmeta.es.util.fuzzer

import esmeta.es.*
import esmeta.es.util.*
import esmeta.es.util.Coverage.*
import esmeta.spec.Grammar
import esmeta.util.*
import esmeta.util.BaseUtils.*

/** mutation target selector for fuzzer */
trait TargetSelector {

  /** target selection */
  def apply(
    pool: Iterable[Script],
    cov: Coverage,
  ): (String, Script, Option[CondView], Option[Nearest])
}

/** weighted mutation target selector */
class WeightedSelector(pairs: (TargetSelector, Int)*) extends TargetSelector {
  def apply(
    pool: Iterable[Script],
    cov: Coverage,
  ): (String, Script, Option[CondView], Option[Nearest]) =
    weightedChoose(pairs)(pool, cov)
}

/** branch coverage-based mutation target selector */
object BranchSelector extends TargetSelector {
  def apply(
    pool: Iterable[Script],
    cov: Coverage,
  ): (String, Script, Option[CondView], Option[Nearest]) =
    val (condView, nearest) = choose(cov.targetCondViews)
    cov.getScript(condView).fold(RandomSelector(pool, cov)) {
      (s"BranchTarget - $condView", _, Some(condView), nearest)
    }
}

/** random mutation target selector */
object RandomSelector extends TargetSelector {
  def apply(
    pool: Iterable[Script],
    cov: Coverage,
  ): (String, Script, Option[CondView], Option[Nearest]) =
    ("RandomTarget", choose(pool), None, None)
}
