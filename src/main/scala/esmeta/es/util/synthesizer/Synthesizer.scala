package esmeta.es.util.synthesizer

import esmeta.cfg.*
import esmeta.es.*
import esmeta.es.util.*
import esmeta.spec.Grammar
import esmeta.spec.util.GrammarGraph
import esmeta.spec.util.GrammarGraph.*

/** ECMAScript AST synthesizer */
trait Synthesizer {

  /** synthesizer name */
  def name: String

  /** get script */
  def script: String

  /** get initial pool */
  def initPool: Vector[String]

  /** for general production */
  def apply(ast: Ast): Ast = ast match
    case ast: Syntactic => apply(ast)
    case ast: Lexical   => apply(ast)

  /** for syntactic production */
  def apply(name: String, args: List[Boolean]): Syntactic
  def apply(ast: Syntactic): Syntactic = apply(ast.name, ast.args)

  /** for lexical production */
  def apply(name: String): Lexical
  def apply(ast: Lexical): Lexical = apply(ast.name)

  /** ECMAScript grammar */
  final lazy val grammar: Grammar = cfg.grammar

  // grammar graph
  final lazy val graph = cfg.grammarGraph
}
