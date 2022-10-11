package esmeta.es.util.mutator

import esmeta.es.*
import esmeta.es.util.synthesizer.*
import esmeta.es.util.{Walker => AstWalker}
import esmeta.es.util.Coverage.*
import esmeta.spec.Grammar
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.ty.AstSingleTy

/** A nearest ECMAScript AST mutator */
class WeightedMutator(
  val pairs: (Mutator, Int)*,
) extends Mutator {

  /** mutate programs */
  def apply(
    ast: Ast,
    condView: Option[CondView],
    nearest: Option[Nearest],
  ): (String, Ast) = weightedChoose(pairs)(ast, condView, nearest)

  val names = pairs.toList.flatMap(_._1.names).sorted.distinct
}
