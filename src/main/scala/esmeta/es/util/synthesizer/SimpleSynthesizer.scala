package esmeta.es.util.synthesizer

import esmeta.cfg.*
import esmeta.es.*
import esmeta.spec.*
import esmeta.spec.util.GrammarGraph
import esmeta.util.*
import esmeta.util.BaseUtils.*
import scala.collection.mutable.Queue
import scala.math.Ordering.Implicits._

/** A simple ECMAScript AST synthesizer */
class SimpleSynthesizer(
  val cfg: CFG,
) extends Synthesizer {
  import grammar.*
  import SimpleSynthesizer.*, GrammarGraph.*

  /** get script */
  def script: String = choose(initPool)

  /** get initial pool */
  lazy val initPool: Vector[String] =
    lazy val pool = (for {
      (node, scripts) <- scriptCovered.toList.sortBy(_._1.id)
      // XXX _ = println(s"- $node:")
      ast <- scripts
      code = ast.toString(grammar)
      // XXX _ = println(s"    $code")
    } yield code).toSet.toVector.sortBy(_.length)
    // XXX time("initPool", pool)
    // XXX println(pool.size)
    pool

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

  private lazy val scriptCovered = getCoveredFrom(graph.getSyn("Script", Nil))

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
      nt <- symbol.getNt
      child = if (nt.optional) None else Some(auxSymbol(map, nt, argMap)._2)
    } yield child
    val ast = Syntactic(name, args, rhsIdx, children)
    val code = ast.toString(grammar)
    (ast, code)

  private def auxSymbol(
    map: Map[Node, (Ast, String)],
    nt: Nonterminal,
    argMap: Map[String, Boolean],
  ): (ProdNode, Ast) =
    val Nonterminal(name, args, optional) = nt
    val prod = nameMap(name)
    if (prod.kind == ProductionKind.Lexical)
      (graph.getLex(name), Lexical(name, reservedLexicals(name)))
    else
      import NonterminalArgumentKind.*
      val newArgs = for (arg <- args) yield arg.kind match
        case True  => true
        case False => false
        case Pass  => argMap(arg.name)
      val synNode = graph.getSyn(name, newArgs)
      val (ast, _) = map(synNode)
      (synNode, ast)

  private def getCoveredFrom(node: SynNode): Map[RhsNode, Vector[Ast]] = {
    val worklist: Worklist[(SynNode, Int, Syntactic => Syntactic)] =
      QueueWorklist(List((node, 0, (x: Syntactic) => x)))
    var map: Map[RhsNode, Vector[Ast]] = Map()
    var minSizes: Map[SynNode, Int] = Map()

    def aux(
      rhsNode: RhsNode,
      rhs: Rhs,
      argMap: Map[String, Boolean],
      astF: Syntactic => Syntactic,
    ): Unit = {
      val RhsNode(_, name, args, rhsIdx) = rhsNode
      var scripts = Vector[Ast]()
      val nts = rhs.symbols.flatMap(_.getNt).toVector
      val opts = for ((nt, i) <- nts.zipWithIndex if nt.optional) yield i
      // added scripts for current RHS node
      val (prodNodes, children) = (for {
        nt <- nts
        (prodNode, child) = auxSymbol(cache, nt, argMap)
      } yield (prodNode, Some(child))).unzip
      def create(children: Vector[Option[Ast]]): Syntactic =
        astF(Syntactic(name, args, rhsIdx, children))
      for {
        removed <- opts.toSet.subsets
        newChildren = for {
          (child, idx) <- children.zipWithIndex
        } yield if (removed contains idx) None else child
        ast = create(newChildren.toVector)
      } scripts :+= ast
      map += rhsNode -> scripts
      // propagate to symbols
      val minChildren =
        for { (nt, child) <- nts zip children } yield
          if (nt.optional) None else child
      val size = create(minChildren).toString(grammar).length
      for {
        (synNode @ SynNode(_, _, _), idx) <- prodNodes.zipWithIndex
        newAstF = (ast: Ast) => create(minChildren.updated(idx, Some(ast)))
      } minSizes.get(synNode) match
        case Some(origSize) if origSize <= size =>
        case _ =>
          minSizes += synNode -> size
          worklist += ((synNode, size, newAstF))
    }

    while (
      worklist.next match
        case Some((SynNode(_, name, args), _, astF)) =>
          val prod = nameMap(name)
          val argMap = (prod.lhs.params zip args).toMap
          for {
            rhsIdx <- Range(0, prod.rhsVec.length)
            rhs = prod.rhsVec(rhsIdx)
            rhsNode = graph.getRhs(name, args, rhsIdx)
          } aux(rhsNode, rhs, argMap, astF)
          true
        case None => false
    ) {}

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
