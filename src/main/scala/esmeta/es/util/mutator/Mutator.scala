package esmeta.es.util.mutator

import esmeta.es.*
import esmeta.es.util.*
import esmeta.es.util.Coverage.*
import esmeta.spec.Grammar
import esmeta.util.*

/** ECMAScript AST mutator */
trait Mutator {

  /** mutate programs */
  def apply(
    code: String,
    debug: Boolean,
  ): Ast = apply(code, None, None, debug)

  /** mutate programs */
  def apply(
    code: String,
    condView: Option[CondView],
    nearest: Option[Nearest],
    debug: Boolean,
  ): Ast = apply(cfg.scriptParser.from(code), condView, nearest, debug)

  /** mutate programs */
  def apply(
    ast: Ast,
    debug: Boolean,
  ): Ast = apply(ast, None, None, debug)

  /** mutate programs */
  def apply(
    ast: Ast,
    condView: Option[CondView],
    nearest: Option[Nearest],
    debug: Boolean,
  ): Ast =
    val (name, mutated) = mutate(ast, condView, nearest)
    val code = mutated.toString(cfg.grammar)
    if (debug)
      println(f"----- $name%-20s-----> $code")
    mutated

  def mutate(
    ast: Ast,
    condView: Option[CondView],
    nearest: Option[Nearest],
  ): (String, Ast)
}
