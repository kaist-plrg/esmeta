package esmeta.es.util.mutator

import esmeta.cfg.CFG
import esmeta.es.*
import esmeta.es.util.*
import esmeta.es.util.Coverage.*
import esmeta.spec.Grammar
import esmeta.util.*

/** ECMAScript AST mutator */
trait Mutator {

  /** mutate programs */
  def apply(
    cfg: CFG,
    code: String,
    debug: Boolean,
  ): Ast = apply(cfg, code, None, None, debug)

  /** mutate programs */
  def apply(
    cfg: CFG,
    code: String,
    condView: Option[CondView],
    nearest: Option[Nearest],
    debug: Boolean,
  ): Ast = apply(cfg, cfg.scriptParser.from(code), condView, nearest, debug)

  /** mutate programs */
  def apply(
    cfg: CFG,
    ast: Ast,
    debug: Boolean,
  ): Ast = apply(cfg, ast, None, None, debug)

  /** mutate programs */
  def apply(
    cfg: CFG,
    ast: Ast,
    condView: Option[CondView],
    nearest: Option[Nearest],
    debug: Boolean,
  ): Ast =
    val (name, mutated) = mutate(cfg, ast, condView, nearest)
    val code = mutated.toString(cfg.grammar)
    if (debug)
      println(f"----- $name%-20s-----> $code")
    mutated

  def mutate(
    cfg: CFG,
    ast: Ast,
    condView: Option[CondView],
    nearest: Option[Nearest],
  ): (String, Ast)
}
