package esmeta.es.util.mutator

import esmeta.cfg.CFG
import esmeta.es.*
import esmeta.es.util.*
import esmeta.spec.Grammar

/** ECMAScript AST mutator */
trait Mutator extends Walker {
  def apply(code: String): Ast = walk(cfg.scriptParser.from(code))
  def apply(ast: Ast): Ast = walk(ast)

  /** control flow graph for ECMA-262 */
  def cfg: CFG

  /** ECMAScript grammar */
  val grammar: Grammar = cfg.grammar

  /** mutator builder */
  def builder: Mutator.Builder

  /** mutator name */
  def name: String = builder.name
}
object Mutator:
  trait Builder extends (CFG => Mutator) { val name: String }
