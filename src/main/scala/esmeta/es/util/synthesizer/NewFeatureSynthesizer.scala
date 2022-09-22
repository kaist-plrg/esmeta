package esmeta.es.util.synthesizer

import esmeta.es.util.GrammarDiff
import esmeta.spec.{Grammar, Production, Rhs}

class NewFeatureSynthesizer(grammar: Grammar)
  extends RandomSynthesizer(grammar) {
  override protected def chooseRhs(
    prod: Production,
    pairs: Iterable[(Rhs, Int)],
  ): (Rhs, Int) = GrammarDiff.chooseRhs(prod, pairs)
}

object NewFeatureSynthesizer extends Synthesizer.Builder {
  val name: String = "NewFeatureSynthesizer"

  def apply(grammar: Grammar) = new NewFeatureSynthesizer(grammar)
}
