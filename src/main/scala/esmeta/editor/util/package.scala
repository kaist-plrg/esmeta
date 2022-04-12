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
import esmeta.ir.{Func => IRFunc, NAME_THIS}
import esmeta.editor.sview.*

/** measure statement coverage of given JS program */
def measureCoverage(
  cfg: CFG,
  sourceText: String,
  cachedAst: Option[Ast] = None,
  checkExit: Boolean = true,
): Set[Int] = {
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
}

/** extension for ast */
extension (ast: Ast) {

  /** get all parents */
  def getParents: List[Ast] = ast.parent match
    case None         => List(ast)
    case Some(parent) => ast :: parent.getParents

  /** filter by node id */
  def covered(cfg: CFG, sview: SyntacticView, nid: Int): Boolean =
    if (ast contains sview) {
      val sourceText = ast.toString(grammar = Some(cfg.grammar))
      val st = Initialize(cfg, sourceText, Some(ast))

      // run interp
      var result = false
      new Interp(st, Nil) {
        override def step: Boolean = if (result) false else super.step
        override def interp(node: Node): Unit =
          if (node.id == nid) {
            val contexts = this.st.context :: this.st.callStack.map(_.context)
            contexts.flatMap(_.astOpt) match
              case a :: _ =>
                for { parent <- a.getParents }
                  if (parent matches sview) result = true
              case _ =>
          }
          super.interp(node)
      }.fixpoint
      result
    } else false

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
