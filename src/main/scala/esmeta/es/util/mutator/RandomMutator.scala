package esmeta.es.util.mutator

import esmeta.es.*
import esmeta.es.util.{Walker => AstWalker}
import esmeta.es.util.synthesizer.*
import esmeta.spec.Grammar
import esmeta.util.BaseUtils.*

/** A random ECMAScript AST mutator */
class RandomMutator(
  val grammar: Grammar,
  val synBuilder: Synthesizer.Builder = RandomSynthesizer,
) extends Mutator {
  override def walk(ast: Syntactic): Syntactic = ast.name match
    case "AssignmentExpression" | "PrimaryExpression" | "Statement" |
        "VariableDeclaration" if randBool =>
      synthesizer(ast)
    case _ =>
      super.walk(ast)

  /** ECMAScript grammar */
  def builder: Mutator.Builder = RandomMutator

  /** synthesizer */
  val synthesizer = synBuilder(grammar)
}
object RandomMutator extends Mutator.Builder:
  val name: String = "RandomMutator"
  def apply(grammar: Grammar) = new RandomMutator(grammar)
  def apply(
    grammar: Grammar,
    synBuilder: Synthesizer.Builder,
  ) = new RandomMutator(grammar, synBuilder)
