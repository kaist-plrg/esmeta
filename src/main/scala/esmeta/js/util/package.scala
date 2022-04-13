package esmeta.js.util

import esmeta.js.*
import esmeta.cfg.CFG

/** merge statements to script */
// TODO refactoring
def mergeStmt(l: List[Ast]): Ast =
  val params = List(false, false, false)
  val bodyOpt = l match
    case a :: rest =>
      val init: Ast = Syntactic("StatementList", params, 0, List(Some(a)))
      val list = rest.foldLeft(init) {
        case (x, y) =>
          Syntactic("StatementList", Nil, 1, List(Some(x), Some(y)))
      }
      Some(Syntactic("ScriptBody", Nil, 0, List(Some(list))))
    case Nil => None
  Syntactic("Script", Nil, 0, List(bodyOpt))

/** flatten statements */
// TODO refactoring
def flattenStmtList(
  s: Ast,
  list: List[Ast] = Nil,
): List[Ast] = s match
  case Syntactic("StatementList", _, 0, List(Some(x0))) =>
    x0 :: list
  case Syntactic("StatementList", _, 1, List(Some(x0), Some(x1))) =>
    flattenStmtList(x0, x1 :: list)
  case _ => Nil
def flattenStmt(s: Ast): List[Ast] = s match
  case Syntactic("Script", _, 0, List(Some(body))) =>
    body match
      case Syntactic("ScriptBody", _, 0, List(Some(stmtList))) =>
        flattenStmtList(stmtList)
      case _ => Nil
  case _ => Nil

/** extensions for ast */
extension (ast: Ast) {

  /** dump */
  def dumpTo(filename: String): Unit = ???

  /** get cover map */
  def coverMap(cfg: CFG): Map[Ast, Ast] = ast match {
    case syn @ Syntactic(name, args, _, children) =>
      val currentMap = syn.parent match {
        case Some(parent: Syntactic) =>
          inline def getParser(pname: String) = Some(cfg.jsParser(pname, args))
          val parserOpt = name match {
            case "CoverParenthesizedExpressionAndArrowParameterList" =>
              parent.name match
                case "PrimaryExpression" => getParser("ParenthesizedExpression")
                case "ArrowParameters"   => getParser("ArrowFormalParameters")
                case _                   => None
            case "CoverCallExpressionAndAsyncArrowHead" =>
              parent.name match
                case "CallExpression"     => getParser("CallMemberExpression")
                case "AsyncArrowFunction" => getParser("AsyncArrowHead")
                case _                    => None
            // AssignmentExpression: LeftHandSideExpression = AssignmentExpression
            // DestructuringAssignmentTarget: LeftHandSideExpression
            case "LeftHandSideExpression" =>
              parent.name match
                case "AssignmentExpression" if parent.rhsIdx == 4 =>
                  if (
                    syn.types.contains("ArrayLiteral") ||
                    syn.types.contains("ObjectLiteral")
                  ) getParser("AssignmentPattern")
                  else None
                case "DestructuringAssignmentTarget" =>
                  getParser("AssignmentPattern")
                case _ => None
            case _ => None
          }
          parserOpt.fold(Map())(parser => {
            val reparsed =
              parser.from(syn.toString(grammar = Some(cfg.grammar)))
            reparsed.coverMap(cfg) + (syn -> reparsed)
          })
        case _ => Map()
      }
      children.foldLeft(currentMap) {
        case (m, Some(child)) => m ++ child.coverMap(cfg)
        case (m, None)        => m
      }
    case _ => Map()
  }
}
