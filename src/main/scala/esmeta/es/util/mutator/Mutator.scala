package esmeta.es.util.mutator

import esmeta.es.*
import esmeta.es.util.*
import esmeta.spec.Grammar

/** ECMAScript AST mutator */
trait Mutator extends Walker {
  def apply(ast: Ast): Ast = walk(ast)

  /** ECMAScript grammar */
  def grammar: Grammar

  /** mutator builder */
  def builder: Mutator.Builder

  /** mutator name */
  def name: String = builder.name
}
object Mutator:
  trait Builder extends (Grammar => Mutator) { val name: String }
