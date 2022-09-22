package esmeta.es.util.synthesizer

import esmeta.cfg.*
import esmeta.error.*
import esmeta.es.*
import esmeta.spec.*
import esmeta.util.BaseUtils.*

/** A weighted ECMAScript AST synthesizer */
abstract class WeightedSynthesizer(
  val pairs: Array[(Synthesizer, Int)],
  val cfg: CFG,
) extends Synthesizer {
  import grammar.*
  import WeightedSynthesizer.*

  /** get script */
  def script: String = weightedChoose(pairs).script

  /** get initial pool */
  lazy val initPool: Vector[String] = (for {
    (syn, _) <- pairs
    code <- syn.initPool
  } yield code).toVector

  /** for syntactic production */
  def apply(name: String, args: List[Boolean]): Syntactic =
    throw NotSupported("WeightedSynthesizer.apply")

  /** for lexical production */
  def apply(name: String): Lexical =
    throw NotSupported("WeightedSynthesizer.apply")

  /** synthesizer builder */
  def builder: SynBuilder
}
object WeightedSynthesizer {
  def apply(pairs: (SynBuilder, Int)*): SynBuilder =
    lazy val _builder: SynBuilder = new SynBuilder {
      val name: String = "WeightedSynthesizer"
      def apply(cfg: CFG) =
        val ps = (for ((builder, int) <- pairs) yield (builder(cfg), int))
        new WeightedSynthesizer(ps.toArray, cfg) {
          def builder: SynBuilder = _builder
        }
    }
    _builder
}
