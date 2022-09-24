package esmeta.es.util.synthesizer

import esmeta.cfg.*
import esmeta.es.*
import esmeta.spec.*
import esmeta.spec.util.GrammarGraph
import esmeta.util.*
import esmeta.util.BaseUtils.*

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
  lazy val initPool: Vector[String] = ???

  /** for syntactic production */
  def apply(name: String, args: List[Boolean]): Syntactic =
    val (ast, _) = cache(graph.getSyn(name, args))
    ast.asInstanceOf[Syntactic]

  /** for lexical production */
  def apply(name: String): Lexical =
    Lexical(name, reservedLexicals(name))

  /** synthesizer builder */
  def builder: Synthesizer.Builder = SimpleSynthesizer

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  // grammar graph
  val graph = cfg.grammarGraph

  // cache for shortest AST for each grammar node
  private lazy val cache: Map[Node, (Ast, String)] = {
    var map: Map[Node, (Ast, String)] = Map()
    val minRhsIdx = graph.minRhsIdx
    val nameMap = cfg.grammar.nameMap
    val worklist = QueueWorklist(graph.topological)

    def update(node: Node, pair: (Ast, String)): Unit =
      val (_, code) = pair
      // node match
      //   case SynNode(_, "IdentifierReference", List(true, true)) |
      //       RhsNode(_, "IdentifierReference", List(true, true), _) =>
      //     println((node, pair, map.get(node)))
      //   case _ =>
      map.get(node) match
        case Some((_, origCode))
            if (
              origCode.length < code.length ||
              (origCode.length == code.length && origCode <= code)
            ) =>
        case _ =>
          map += node -> pair
          node match
            case prodNode: ProdNode =>
              worklist ++= graph.mustUsedIn.getOrElse(prodNode, Set())
            case RhsNode(_, name, args, _) =>
              update(graph.getSyn(name, args), pair)

    def auxNode(node: Node): Unit = node match
      case synNode: SynNode => auxSyn(synNode)
      case lexNode: LexNode => auxLex(lexNode)
      case rhsNode: RhsNode => auxRhs(rhsNode)

    def auxSyn(synNode: SynNode): Unit =
      val SynNode(_, name, args) = synNode
      val rhsIdx = minRhsIdx(synNode)
      val rhsNode = graph.getRhs(name, args, rhsIdx)
      update(synNode, map(rhsNode))

    def auxLex(lexNode: LexNode): Unit =
      val LexNode(_, name) = lexNode
      val code = reservedLexicals(name)
      update(lexNode, (Lexical(name, code), code))

    def auxRhs(rhsNode: RhsNode): Unit =
      val RhsNode(_, name, args, rhsIdx) = rhsNode
      val prod = nameMap(name)
      var argMap = (prod.lhs.params zip args).toMap
      val rhs = prod.rhsVec(rhsIdx)
      val children = for {
        symbol <- rhs.symbols.toVector
        ast <- auxSymbol(symbol, argMap)
      } yield ast
      val ast = Syntactic(name, args, rhsIdx, children)
      val code = ast.toString(grammar)
      update(rhsNode, (ast, code))

    def auxSymbol(
      symbol: Symbol,
      argMap: Map[String, Boolean],
    ): Option[Option[Ast]] = symbol match
      case ButNot(nt, _) => auxSymbol(nt, argMap)
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

    while (worklist.next.map(auxNode).isDefined) {}

    map
  }
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
