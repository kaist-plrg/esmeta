package esmeta.es.util.mutator

import esmeta.es.*
import esmeta.es.util.*
import esmeta.es.util.Coverage.*
import esmeta.spec.Grammar
import esmeta.util.*

/** ECMAScript AST mutator */
trait Mutator {

  /** mutate programs */
  def apply(code: String): (String, Ast) = apply(code, None)

  /** mutate programs */
  def apply(
    code: String,
    target: Option[(CondView, Coverage)],
  ): (String, Ast) = apply(cfg.scriptParser.from(code), target)

  /** mutate programs */
  def apply(ast: Ast): (String, Ast) = apply(ast, None)

  /** mutate programs */
  def apply(
    ast: Ast,
    target: Option[(CondView, Coverage)],
  ): (String, Ast)

  /** Possible names of underlying mutators */
  val names: List[String]
}
