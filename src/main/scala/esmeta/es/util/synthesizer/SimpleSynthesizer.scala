package esmeta.es.util.synthesizer

import esmeta.cfg.*
import esmeta.es.*
import esmeta.spec.*
import esmeta.util.BaseUtils.*

// TODO refactoring
/** A simple ECMAScript AST synthesizer */
class SimpleSynthesizer(
  val cfg: CFG,
) extends Synthesizer {
  import grammar.*
  import SimpleSynthesizer.*

  /** get script */
  def script: String = "x"

  /** get initial pool */
  lazy val initPool: Vector[String] = ???

  /** for syntactic production */
  def apply(name: String, args: List[Boolean]): Syntactic = synNt(name, args)

  /** for lexical production */
  def apply(name: String): Lexical = Lexical(name, reservedLexicals(name))

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  private val synNt = cached[(String, List[Boolean]), Syntactic] {
    case target @ (name, args) =>
      if (visiting contains target) error(s"visiting $target")
      visiting += target
      val prod @ Production(lhs, _, _, rhsVec) = nameMap(name)
      val argsMap = (lhs.params zip args).toMap
      val syns = for {
        (rhs, rhsIdx) <- rhsVec.zipWithIndex
        if rhs.condition.fold(true)(cond => argsMap(cond.name) == cond.pass)
        children <- optional(rhs.symbols.flatMap(synSymbol(argsMap)))
        syn = Syntactic(name, args, rhsIdx, children.toVector)
      } yield syn
      visiting -= target
      syns.minBy(_.toString(grammar).length)
  }

  private var visiting: Set[(String, List[Boolean])] = Set()

  private def synSymbol(argsMap: Map[String, Boolean])(
    symbol: Symbol,
  ): Option[Option[Ast]] = symbol match
    case ButNot(nt, _) => synSymbol(argsMap)(nt)
    case Nonterminal(name, args, optional) =>
      if (reservedLexicals contains name)
        Some(Some(Lexical(name, reservedLexicals(name))))
      else if (optional) Some(None)
      else {
        import NonterminalArgumentKind.*
        val newArgs = for (arg <- args) yield arg.kind match
          case True  => true
          case False => false
          case Pass  => argsMap(arg.name)
        Some(Some(synNt(name, newArgs)))
      }
    case _ => None

  // for correct order
  synNt("Statement", List(false, false, false))

  /** synthesizer builder */
  def builder: Synthesizer.Builder = SimpleSynthesizer
}
object SimpleSynthesizer extends Synthesizer.Builder {
  val name: String = "SimpleSynthesizer"
  def apply(cfg: CFG) = new SimpleSynthesizer(cfg)
  val reservedLexicals: Map[String, String] = Map(
    "IdentifierName" -> "x",
    "NullLiteral" -> "null",
    "BooleanLiteral" -> "true",
    "NumericLiteral" -> "42",
    "StringLiteral" -> "''",
    "NoSubstitutionTemplate" -> "``",
    "TemplateHead" -> "`${",
    "TemplateMiddle" -> "}${",
    "TemplateTail" -> "}`",
    "RegularExpressionLiteral" -> "/a/",
    "PrivateIdentifier" -> "#x",
  )
}
