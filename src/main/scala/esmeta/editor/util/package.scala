package esmeta.editor.util

import esmeta.editor.sview.*
import esmeta.js.{Ast, Syntactic => JsSyntactic, Lexical => JsLexical}
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec

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
  // TODO handle annotation
  def getConcreteParts(sview: SyntacticView): List[Ast] =
    val matched: ListBuffer[Ast] = ListBuffer()
    val concretes: ListBuffer[Ast] = ListBuffer()

    @tailrec
    def aux(astList: List[Ast]): Unit = if (!astList.isEmpty) {
      var next: List[Ast] = List()
      for { ast0 <- astList } {
        if (ast0 matches sview) matched += ast0
        ast0 match
          case jsSyn: JsSyntactic =>
            for {
              childOpt <- jsSyn.children
              child <- childOpt
            } next ::= child
          case _ =>
      }
      aux(next)
    }

    @tailrec
    def aux2(queue: List[(Ast, SyntacticView)]): Unit = if (!queue.isEmpty) {
      var next: List[(Ast, SyntacticView)] = List()
      for { (ast0, sview0) <- queue } (ast0, sview0) match
        case (jsLex: JsLexical, absLex: Lexical) =>
          concretes += jsLex
        case (jsSyn: JsSyntactic, absSyn: Syntactic) =>
          concretes += jsSyn
          (jsSyn.children.flatten zip absSyn.children.flatten).foreach {
            case (jsChild, absChild) => next ::= (jsChild, absChild)
          }
        case _ =>
      aux2(next)
    }

    aux(List(ast))
    aux2(matched.toList.map((_, sview)))
    concretes.toList

  /** check whether given JS ast matches syntactic view */
  // TODO handle annotation
  def matches(sview: SyntacticView): Boolean = (ast, sview) match
    case (_, AbsSyntactic(absName, _, _)) => ast.name == absName
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
