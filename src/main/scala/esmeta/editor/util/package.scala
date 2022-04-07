package esmeta.editor.util

import esmeta.error.*
import esmeta.interp.*
import esmeta.cfg.*
import esmeta.util.BaseUtils.*
import esmeta.js.{
  Ast,
  Initialize,
  Syntactic => JsSyntactic,
  Lexical => JsLexical,
}
import esmeta.editor.sview.*
import scala.annotation.tailrec

/** measure statement coverage of given JS program */
def measureCoverage(
  cfg: CFG,
  sourceText: String,
  cachedAst: Option[Ast] = None,
  checkExit: Boolean = true,
): Set[Int] =
  val st = Initialize(cfg, sourceText, cachedAst)
  var touched: Set[Int] = Set()

  // run interp
  new Interp(st, Nil) {
    override def interp(node: Node): Unit =
      touched += node.id
      super.interp(node)
  }.fixpoint

  // check exit and return result
  if (checkExit) st(GLOBAL_RESULT) match
    case comp: Comp if comp.ty == CONST_NORMAL =>
    case v                                     => error(s"not normal exit")
  touched

/** extension for ast */
extension (ast: Ast) {

  /** check whether given JS program contains sytactic view */
  def contains(sview: SyntacticView): Boolean = {
    // find actual root of syntactic view
    @tailrec
    def getRoot(sv: SyntacticView): SyntacticView = sv match
      case Syntactic(_, _, _, List(Some(child))) => getRoot(child)
      case _                                     => sv
    val sviewRoot = getRoot(sview)

    // check if given ast and sview are exactly matched
    def matched(a: Ast, s: SyntacticView): Boolean = (a, s) match
      case (_, AbsSyntactic(absName)) => a.name == absName
      case (jsLex: JsLexical, absLex: Lexical) =>
        jsLex.name == absLex.name &&
        jsLex.str.trim == absLex.str.trim
      case (jsSyn: JsSyntactic, absSyn: Syntactic) =>
        jsSyn.name == absSyn.name &&
        jsSyn.rhsIdx == absSyn.rhsIdx &&
        (jsSyn.children zip absSyn.children).forall {
          case (None, None)                    => true
          case (Some(jsChild), Some(absChild)) => matched(jsChild, absChild)
          case _                               => false
        }
      case _ => false

    // aux function for contains
    def aux(a: Ast, s: SyntacticView): Boolean =
      if (matched(a, s)) true
      else
        a match
          case jsSyn: JsSyntactic =>
            jsSyn.children.foldLeft(false) {
              case (true, _)            => true
              case (false, None)        => false
              case (false, Some(child)) => aux(child, s)
            }
          case _ => false

    aux(ast, sviewRoot)
  }
}
