package esmeta.es.util.mutator

import esmeta.es.*
import esmeta.es.util.*
import esmeta.es.util.Coverage.*
import esmeta.spec.Grammar
import esmeta.util.*

/** ECMAScript AST mutator */
trait Mutator {

  /** mutate programs */
  def apply(code: String): (String, Ast) = apply(code, None, None)

  /** mutate programs */
  def apply(
    code: String,
    condView: Option[CondView],
    nearest: Option[Nearest],
  ): (String, Ast) = apply(cfg.scriptParser.from(code), condView, nearest)

  /** mutate programs */
  def apply(
    ast: Ast,
  ): (String, Ast) = apply(ast, None, None)

  /** mutate programs */
  def apply(
    ast: Ast,
    condView: Option[CondView],
    nearest: Option[Nearest],
  ): (String, Ast)

  /** Possible names of underlying mutators */
  val names: List[String]
}
