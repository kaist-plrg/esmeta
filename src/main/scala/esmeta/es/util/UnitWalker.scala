package esmeta.es.util

import esmeta.util.BasicUnitWalker
import esmeta.es.*

/** a unit walker for ECMAScript */
trait UnitWalker extends BasicUnitWalker {
  def walk(elem: ESElem): Unit = elem match
    case elem: Script      => walk(elem)
    case elem: Ast         => walk(elem)
    case elem: ConformTest => walk(elem)
    case elem: Assertion   => walk(elem)

  /** ECMAScript script program */
  def walk(script: Script): Unit = walk(script.ast)

  /** ASTs */
  def walk(ast: Ast): Unit = ast match
    case ast: Syntactic => walk(ast)
    case ast: Lexical   => walk(ast)

  /** syntactic productions */
  def walk(ast: Syntactic): Unit =
    walkList(ast.children, walkOpt(_, walk))

  /** lexical productions */
  def walk(ast: Lexical): Unit = {}

  /** conformance test */
  def walk(test: ConformTest): Unit = walkVector(test.assertions, walk)

  /** assertions */
  def walk(assert: Assertion): Unit = {}
}
