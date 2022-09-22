package esmeta.es.util.mutator

import esmeta.cfg.CFG
import esmeta.es.*
import esmeta.es.util.synthesizer.*
import esmeta.es.util.{Walker => AstWalker}
import esmeta.spec.Grammar
import esmeta.util.BaseUtils.*

/** A random ECMAScript AST mutator */
class RandomMutator(
  val cfg: CFG,
  val synBuilder: Synthesizer.Builder = RandomSynthesizer,
) extends Mutator {
  override def walk(ast: Syntactic): Syntactic = ast.name match
    case "AssignmentExpression" | "PrimaryExpression" | "Statement" |
        "VariableDeclaration" if randBool =>
      synthesizer(ast)
    case _ =>
      super.walk(ast)

  /** mutator builder */
  def builder: Mutator.Builder = RandomMutator

  /** synthesizer */
  val synthesizer = synBuilder(cfg)
}
object RandomMutator extends Mutator.Builder:
  val name: String = "RandomMutator"
  def apply(cfg: CFG) = new RandomMutator(cfg)
  def apply(
    cfg: CFG,
    synBuilder: Synthesizer.Builder,
  ) = new RandomMutator(cfg, synBuilder)
