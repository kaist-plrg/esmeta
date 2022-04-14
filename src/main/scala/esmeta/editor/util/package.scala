package esmeta.editor.util

import esmeta.js.{Ast, Syntactic => JsSyntactic, Lexical => JsLexical}
import scala.collection.mutable.ListBuffer
import esmeta.editor.sview.*

/** extension for ast */
extension (ast: Ast) {

  /** set id */
  def setId(id: Int): Int = {
    ast.idOpt = Some(id)
    var inc = 1
    ast match
      case syn: JsSyntactic =>
        for {
          childOpt <- syn.children
          child <- childOpt
        } inc += child.setId(id + inc)
      case lex: JsLexical => /* do nothing */
    inc
  }

  /** get size */
  def size: Int = ast match
    case syn: JsSyntactic =>
      1 + syn.children.foldLeft(0) {
        case (acc, Some(child)) => acc + child.size
        case (acc, None)        => acc + 1
      }
    case lex: JsLexical => 1

  /** get ast that matches concrete part of syntactic view */
  def getConcreteParts(sview: SyntacticView): Set[Ast] =
    def aux(ast0: Ast): List[Ast] =
      if (ast0 matches sview) List(ast0)
      else
        ast0 match
          case jsSyn: JsSyntactic =>
            (for {
              childOpt <- jsSyn.children
              child <- childOpt
            } yield aux(child)).flatten
          case _ => List()

    def aux2(ast0: Ast, sview: SyntacticView): List[Ast] = (ast0, sview) match
      case (_, AbsSyntactic(absName))          => List()
      case (jsLex: JsLexical, absLex: Lexical) => List(jsLex)
      case (jsSyn: JsSyntactic, absSyn: Syntactic) =>
        jsSyn :: (jsSyn.children zip absSyn.children).flatMap {
          case (Some(jsChild), Some(absChild)) => aux2(jsChild, absChild)
          case _                               => List()
        }
      case _ => List()
    aux(ast).flatMap(aux2(_, sview)).toSet
  // val matched = aux(ast)
  // println("****************************************")
  // println(matched)
  // val res = matched.flatMap(aux2(_, sview)).toSet
  // println("****************************************")
  // println(res)
  // res

  /** check whether given JS ast matches syntactic view */
  def matches(sview: SyntacticView): Boolean = (ast, sview) match
    case (_, AbsSyntactic(absName, _)) => ast.name == absName
    case (jsLex: JsLexical, absLex: Lexical) =>
      jsLex.name == absLex.name &&
      jsLex.str.trim == absLex.str.trim
    case (jsSyn: JsSyntactic, absSyn: Syntactic) =>
      jsSyn.name == absSyn.name &&
      jsSyn.rhsIdx == absSyn.rhsIdx &&
      (jsSyn.children zip absSyn.children).forall {
        case (None, None)                    => true
        case (Some(jsChild), Some(absChild)) => jsChild matches absChild
        case _                               => false
      }
    case _ => false

  /** check whether given JS ast contains sytactic view */
  def contains(sview: SyntacticView): Boolean =
    if (ast matches sview) true
    else
      ast match
        case jsSyn: JsSyntactic =>
          jsSyn.children.foldLeft(false) {
            case (true, _)            => true
            case (false, None)        => false
            case (false, Some(child)) => child contains sview
          }
        case _ => false
}
