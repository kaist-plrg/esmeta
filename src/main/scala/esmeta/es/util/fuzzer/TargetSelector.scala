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

  /** Possible names of underlying selectors */
  val names: List[String]
}

/** weighted mutation target selector */
class WeightedSelector(pairs: (TargetSelector, Int)*) extends TargetSelector {
  def apply(
    pool: Iterable[Script],
    cov: Coverage,
  ): (String, Script, Option[CondView], Option[Nearest]) =
    weightedChoose(pairs)(pool, cov)

  val names = pairs.toList.flatMap(_._1.names)
}

/** branch coverage-based mutation target selector */
object BranchSelector extends TargetSelector {
  def apply(
    pool: Iterable[Script],
    cov: Coverage,
  ): (String, Script, Option[CondView], Option[Nearest]) =
    val (condView, nearest) = choose(cov.targetCondViews)
    cov.getScript(condView).fold(RandomSelector(pool, cov)) {
      (names.head, _, Some(condView), nearest)
    }

  val names = List("BranchTarget")
}

/** random mutation target selector */
object RandomSelector extends TargetSelector {
  def apply(
    pool: Iterable[Script],
    cov: Coverage,
  ): (String, Script, Option[CondView], Option[Nearest]) =
    (names.head, choose(pool), None, None)

  val names = List("RandomTarget")
}
