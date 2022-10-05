package esmeta.es.util.mutator

import esmeta.es.*
import esmeta.es.util.*
import esmeta.es.util.synthesizer.*
import esmeta.es.util.{Walker => AstWalker}
import esmeta.es.util.Coverage.*
import esmeta.spec.Grammar
import esmeta.util.*
import esmeta.util.BaseUtils.*

/** A mutator that inserts statements to ECMAScript AST */
class StatementInserter(
  val synthesizer: Synthesizer = RandomSynthesizer,
) extends Mutator
  with AstWalker {

  import StatementInserter.*

  /** mutate a program */
  def mutate(
    ast: Ast,
    _condView: Option[CondView],
    _nearest: Option[Nearest],
  ): (String, Ast) =
    ("StatementInserter", walk(ast))

  /** generate a new statement list item, either randomly or manually */
  private def newStmtItem(args: List[Boolean]) = choose(
    synthesizer(STATEMENT_LIST_ITEM, args),
    choose(manualStmtItems(args)),
  )

  /** lift a single statementListItem to statementList */
  private def item2list(item: Ast, args: List[Boolean]) =
    Syntactic(STATEMENT_LIST, args, 0, Vector(Some(item)))

  /** ast walker */
  override def walk(ast: Syntactic): Syntactic = ast match
    // statement list
    case Syntactic(STATEMENT_LIST, args, _, _) if randBool(0.8) =>
      Syntactic(
        STATEMENT_LIST,
        args,
        1,
        Vector(Some(super.walk(ast)), Some(newStmtItem(args))),
      )

    // optional statement list as child
    case Syntactic(name, args, rhsIdx, children)
        if STATEMENT_LIST_OPTIONAL_CONTAINERS.exists {
          case (cName, cRhsIdx, cChildIdx, _) =>
            cName == name &&
            cRhsIdx == rhsIdx &&
            children(cChildIdx) == None
        } && randBool(0.8) =>
      val container = STATEMENT_LIST_OPTIONAL_CONTAINERS.find(_._1 == name).get

      // change args
      val rhsArgModifier = container._4.toList
      val newArgs = rhsArgModifier.zipWithIndex.map {
        case (-1, _) => false
        case (0, i)  => optional(args(i)).getOrElse(false)
        case (1, _)  => true
        case _       => false
      }

      // change children
      val childIdx = container._3
      val newChildren = children.zipWithIndex.map {
        case (c, i) =>
          if (i == childIdx)
            Some(item2list(newStmtItem(newArgs), newArgs))
          else
            c.map(super.walk)
      }

      Syntactic(name, newArgs, rhsIdx, newChildren)
    case _ =>
      super.walk(ast)
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
}
