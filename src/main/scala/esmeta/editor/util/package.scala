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
import scala.collection.mutable.ListBuffer
import esmeta.editor.sview.*

/** extension for ast */
extension (ast: Ast) {

  /** get initialized state */
  def initState(cfg: CFG): State =
    val sourceText = ast.toString(grammar = Some(cfg.grammar))
    Initialize(cfg, sourceText, Some(ast))

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

  /** get touched node ids */
  def touchedNodes(cfg: CFG): Set[Int] = {
    val st = ast.initState(cfg)
    var touched: Set[Int] = Set()

    // run interp
    new Interp(st, Nil) {
      override def interp(node: Node): Unit = {
        touched += node.id; super.interp(node)
      }
    }.fixpoint

    // check exit and return result
    st(GLOBAL_RESULT) match
      case comp: Comp if comp.ty == CONST_NORMAL => touched
      case v                                     => error(s"not normal exit")
  }

  /** get touched algorithms per ast */
  def touchedAlgos(cfg: CFG, nextAstId: Int) = {
    val st = ast.initState(cfg)

    // run interp
    var map: Map[Int, Set[Int]] = Map()
    new Interp(st, Nil) {
      // TODO handle cover grammar

      // save algo id of top-most evaluation
      override def interp(node: Node): Unit = {
        super.interp(node)
        node match {
          case _: Call =>
            // get top-most ast
            val cs = this.st.context :: this.st.callStack.map(_.context)
            val targets = cs.flatMap { c =>
              if (c.name endsWith ".Evaluation") c.astOpt
              else None
            }

            // save algo id of current context
            for {
              ast <- targets.headOption
              astId <- ast.idOpt
              algoIds = map.getOrElse(astId, Set())
            } map += (astId -> (algoIds + this.st.context.func.id))
          case _ => /* do nothing */
        }
      }

      // TODO
      // if current context is evaluation, save type of value
      override def setReturn(value: Value): Unit = {
        super.setReturn(value)
      }
    }.fixpoint

    // check exit and return result
    st(GLOBAL_RESULT) match
      case comp: Comp if comp.ty == CONST_NORMAL => map
      case v                                     => error(s"not normal exit")
  }

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
