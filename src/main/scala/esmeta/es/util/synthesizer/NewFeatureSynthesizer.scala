package esmeta.es.util.synthesizer

import esmeta.cfg.*
import esmeta.es.util.GrammarDiff
import esmeta.spec.{Production, Rhs}

class NewFeatureSynthesizer(cfg: CFG) extends RandomSynthesizer(cfg) {
  override protected def chooseRhs(
    prod: Production,
    pairs: Iterable[(Rhs, Int)],
  ): (Rhs, Int) = GrammarDiff.chooseRhs(prod, pairs)
}

object NewFeatureSynthesizer extends Synthesizer.Builder {
  val name: String = "NewFeatureSynthesizer"
  def apply(cfg: CFG) = new NewFeatureSynthesizer(cfg)
}
