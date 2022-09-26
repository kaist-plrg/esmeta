package esmeta.es.util.synthesizer

import esmeta.cfg.*
import esmeta.es.*
import esmeta.spec.*
import esmeta.spec.util.GrammarGraph
import esmeta.util.*
import esmeta.util.BaseUtils.*
import scala.math.Ordering.Implicits._

// TODO refactoring
/** A simple ECMAScript AST synthesizer */
class SimpleSynthesizer(
  val cfg: CFG,
) extends Synthesizer {
  import grammar.*
  import SimpleSynthesizer.*, GrammarGraph.*

  /** get script */
  def script: String = "x"

  /** get initial pool */
  lazy val initPool: Vector[String] = (for {
    rhsNode @ RhsNode(_, name, args, rhsIdx) <- graph.rhsNodes
    prod = nameMap(name)
    rhs = prod.rhsVec(rhsIdx)
  } yield ???).toVector

  /** for syntactic production */
  def apply(name: String, args: List[Boolean]): Syntactic =
    val (ast, _) = cache(graph.getSyn(name, args))
    ast.asInstanceOf[Syntactic]

  /** for lexical production */
  def apply(name: String): Lexical =
    Lexical(name, reservedLexicals(name))

  /** synthesizer builder */
  def builder: Synthesizer.Builder = SimpleSynthesizer

  // grammar graph
  val graph = cfg.grammarGraph

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  // name map for productions
  private val nameMap = cfg.grammar.nameMap

  // cache for shortest AST for each grammar node
  private lazy val cache: Map[Node, (Ast, String)] =
    graph.fixpoint(Map(), graph.topological, auxNode)

  // lexicographical ordering for code length and code string
  private given Ordering[(Ast, String)] = Ordering.by {
    case (_, code) => (code.length, code)
  }

  private def auxNode(
    map: Map[Node, (Ast, String)],
    node: Node,
  ): (Ast, String) = node match
    case synNode: SynNode => auxSyn(map, synNode)
    case lexNode: LexNode => auxLex(map, lexNode)
    case rhsNode: RhsNode => auxRhs(map, rhsNode)

  private def auxSyn(
    map: Map[Node, (Ast, String)],
    synNode: SynNode,
  ): (Ast, String) =
    val SynNode(_, name, args) = synNode
    val pairs = for {
      rhsIdx <- Range(0, nameMap(name).rhsVec.length)
      rhsNode = graph.getRhs(name, args, rhsIdx)
      pair @ (_, code) <- map.get(rhsNode)
    } yield pair
    pairs.min

  private def auxLex(
    map: Map[Node, (Ast, String)],
    lexNode: LexNode,
  ): (Ast, String) =
    val LexNode(_, name) = lexNode
    val code = reservedLexicals(name)
    (Lexical(name, code), code)

  private def auxRhs(
    map: Map[Node, (Ast, String)],
    rhsNode: RhsNode,
  ): (Ast, String) =
    val RhsNode(_, name, args, rhsIdx) = rhsNode
    val prod = nameMap(name)
    var argMap = (prod.lhs.params zip args).toMap
    val rhs = prod.rhsVec(rhsIdx)
    val children = for {
      symbol <- rhs.symbols.toVector
      ast <- auxSymbol(map, symbol, argMap)
    } yield ast
    val ast = Syntactic(name, args, rhsIdx, children)
    val code = ast.toString(grammar)
    (ast, code)

  private def auxSymbol(
    map: Map[Node, (Ast, String)],
    symbol: Symbol,
    argMap: Map[String, Boolean],
  ): Option[Option[Ast]] = symbol match
    case ButNot(nt, _) => auxSymbol(map, nt, argMap)
    case Nonterminal(name, args, optional) =>
      val prod = nameMap(name)
      if (prod.kind == ProductionKind.Lexical)
        Some(Some(Lexical(name, reservedLexicals(name))))
      else if (optional) Some(None)
      else
        import NonterminalArgumentKind.*
        val newArgs = for (arg <- args) yield arg.kind match
          case True  => true
          case False => false
          case Pass  => argMap(arg.name)
        val (ast, _) = map(graph.getSyn(name, newArgs))
        Some(Some(ast))
    case _ => None
}
object SimpleSynthesizer extends Synthesizer.Builder {
  val name: String = "SimpleSynthesizer"
  def apply(cfg: CFG) = new SimpleSynthesizer(cfg)
  val reservedLexicals: Map[String, String] = Map(
    "BooleanLiteral" -> "true",
    "IdentifierName" -> "x",
    "NoSubstitutionTemplate" -> "``",
    "NullLiteral" -> "null",
    "NumericLiteral" -> "0",
    "PrivateIdentifier" -> "#x",
    "RegularExpressionLiteral" -> "/a/",
    "StringLiteral" -> "''",
    "TemplateHead" -> "`${",
    "TemplateMiddle" -> "}${",
    "TemplateTail" -> "}`",
  )
}
