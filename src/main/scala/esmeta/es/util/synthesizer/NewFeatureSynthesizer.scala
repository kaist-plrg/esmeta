package esmeta.es.util.synthesizer

import esmeta.cfg.*
import esmeta.es.util.GrammarDiff
import esmeta.spec.{Production, Rhs}

object NewFeatureSynthesizer extends NewFeatureSynthesizer
trait NewFeatureSynthesizer extends RandomSynthesizer {

  /** synthesizer name */
  override val name: String = "NewFeatureSynthesizer"

  override protected def chooseRhs(
    prod: Production,
    pairs: Iterable[(Rhs, Int)],
  ): (Rhs, Int) = GrammarDiff.chooseRhs(prod, pairs)
}
