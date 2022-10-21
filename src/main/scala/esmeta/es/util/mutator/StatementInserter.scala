package esmeta.es.util.mutator

import esmeta.es.*
import esmeta.es.util.*
import esmeta.es.util.synthesizer.*
import esmeta.es.util.{UnitWalker => AstWalker, *}
import esmeta.es.util.Coverage.*
import esmeta.spec.Grammar
import esmeta.util.*
import esmeta.util.BaseUtils.*

/** A mutator that inserts statements to ECMAScript AST */
class StatementInserter(
  val synthesizer: Synthesizer = RandomSynthesizer,
) extends Mutator {

  /** mutate a program */
  def apply(
    ast: Ast,
    n: Int,
    _taregt: Option[(CondView, Coverage)],
  ): (String, Seq[Ast]) = (
    names(0), {
      // count the number of stmtLists
      val k = StatementInserter.StmtListCounter(ast)

      if (k == 0) List.fill(n)(ast)
      else if (n == 1)
        c = 2
        k1 = 1
        k2 = k - 1

        sample(ast, n)
      else {
        // calculate the most efficient parameters
        c = 2
        while (math.pow(c, k) < n)
          c = c + 1
        k1 = 0
        k2 = k
        while (math.pow((c - 1), k1 + 1) * math.pow(c, k2 - 1) >= n)
          k1 = k1 + 1
          k2 = k2 - 1

        // choose n distince samples.
        sample(ast, n)
      }

    },
  )

  val names = List("StatementInserter")

  import StatementInserter.*

  /** parameter for sampler */
  private var (c, k1, k2) = (0, 0, 0)
  private def decideGenNum =
    if k1 > 0 && randBool(k1 / (k1 + k2 + 0.0)) then
      k1 -= 1; c - 1
    else if k2 > 0 then
      k2 -= 1; c
    else throw new Error("This is a bug in Stmt Insertion")

  def sample(ast: Ast, n: Int) = shuffle(walk(ast)).take(n)

  /** generate a new statement list item, either randomly or manually */
  private def newStmtItem(args: List[Boolean]) = choose(
    synthesizer(STATEMENT_LIST_ITEM, args),
    choose(manualStmtItems(args)),
  )

  /** lift a single statementListItem to statementList */
  private def item2list(item: Ast, args: List[Boolean]) =
    Syntactic(STATEMENT_LIST, args, 0, Vector(Some(item)))

  /** ast walker */
  def walkOpt(opt: Option[Ast]): List[Option[Ast]] = opt match {
    case None      => List(None)
    case Some(ast) => walk(ast).map(ast => Some(ast))
  }
  def walk(ast: Ast): List[Ast] = ast match
    case ast: Lexical   => walk(ast)
    case ast: Syntactic => walk(ast)
  def walk(ast: Lexical): List[Lexical] = List(ast)
  def walk(ast: Syntactic): List[Syntactic] = ast match
    // statement list
    case Syntactic(STATEMENT_LIST, args, _, _) =>
      val genNum = decideGenNum
      val mutants = superWalk(ast)
      List
        .tabulate(genNum)(_ match
          case 0 => mutants
          case _ =>
            val newStmt = newStmtItem(args)
            mutants.map(mutant =>
              Syntactic(
                STATEMENT_LIST,
                args,
                1,
                Vector(Some(mutant), Some(newStmt)),
              ),
            ),
        )
        .flatten

    // optional statement list as child
    case _ if containsEmptyStatementList(ast) =>
      val Syntactic(name, args, rhsIdx, children) = ast
      val container = STATEMENT_LIST_OPTIONAL_CONTAINERS.find(_._1 == name).get

      // get args for new stmt to be added
      val rhsArgModifier = container._4.toList
      val newArgs = rhsArgModifier.zipWithIndex.map {
        case (-1, _) => false
        case (0, i)  => optional(args(i)).getOrElse(false)
        case (1, _)  => true
        case _       => false
      }

      // generate new stmts
      val genNum = decideGenNum
      val newStmts = List.tabulate(genNum)(_ match
        case 0 => None
        case _ => Some(item2list(newStmtItem(newArgs), newArgs)),
      )

      // change children
      val childIdx = container._3
      val newChildrens =
        children.zipWithIndex.foldRight(List(Vector[Option[Ast]]())) {
          case ((child, i), childrens) => {
            for {
              child <- if (i == childIdx) newStmts else walkOpt(child)
              children <- childrens
            } yield (child +: children)
          }
        }
      newChildrens.map(newChildren =>
        Syntactic(name, args, rhsIdx, newChildren),
      )

    case _ => superWalk(ast)

  def superWalk(ast: Syntactic) =
    val Syntactic(name, args, rhsIdx, children) = ast
    val newChildrens = children
      .map(walkOpt)
      .foldRight(List(Vector[Option[Ast]]()))((childs, childrens) => {
        for {
          child <- childs
          childrens <- childrens
        } yield (child +: childrens)
      })
    newChildrens.map(newChildren => Syntactic(name, args, rhsIdx, newChildren))
}

object StatementInserter {
  val STATEMENT_LIST = "StatementList"
  val STATEMENT_LIST_ITEM = "StatementListItem"
  val STATEMENT_LIST_OPTIONAL_CONTAINERS = List(
    // (name, rhsIdx, childIdx of optional statementList, rhs arg modifier)
    // TODO: automatically generate this list
    ("Block", 0, 0, (0, 0, 0)),
    ("CaseClause", 0, 1, (0, 0, 0)),
    ("DefaultClause", 0, 0, (0, 0, 0)),
    ("FunctionStatementList", 0, 0, (0, 0, 1)),
    ("ClassStaticBlockStatementList", 0, 0, (-1, 1, -1)),
  )

  def containsEmptyStatementList(ast: Syntactic) =
    val Syntactic(name, args, rhsIdx, children) = ast

    STATEMENT_LIST_OPTIONAL_CONTAINERS.exists {
      case (cName, cRhsIdx, cChildIdx, _) =>
        cName == name &&
        cRhsIdx == rhsIdx &&
        children(cChildIdx) == None
    }

  val manualStmts = List(
    "x ( ) ; ",
    "x ( 0 ) ; ",
    "return ; ",
    "return 0 ; ",
    "throw 0 ; ",
    "yield 0 ; ",
    "await x ( ) ; ",
    "yield * x ( ) ; ",
    "new x ( ) ; ",
    "break ; ",
    "continue ; ",
  )
  // TODO: generalize to case where length of args is not 3
  lazy val manualStmtItems: Map[List[Boolean], List[Ast]] = (
    for (
      a1 <- List(true, false);
      a2 <- List(true, false);
      a3 <- List(true, false);
      args = List(a1, a2, a3)
    )
      yield (
        args,
        manualStmts.flatMap(code =>
          optional(cfg.esParser(STATEMENT_LIST_ITEM, args).from(code)),
        ),
      )
  ).toMap

  // count the number of places where stmt can be inserted
  object StmtListCounter extends AstWalker {
    def apply(ast: Ast): Int = {
      _cnt = 0
      walk(ast)
      _cnt
    }
    private var _cnt = 0

    override def walk(ast: Syntactic): Unit = {
      ast match
        case Syntactic(STATEMENT_LIST, _, _, _) =>
          _cnt += 1
        case _ if containsEmptyStatementList(ast) =>
          _cnt += 1
        case _ =>
      super.walk(ast)
    }

  }
}
