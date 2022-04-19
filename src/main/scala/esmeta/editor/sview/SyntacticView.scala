package esmeta.editor.sview

import esmeta.js.Ast
import esmeta.editor.*
import esmeta.cfg.CFG
import esmeta.editor.util.CFGHelper

// syntactic view: can be changed at future
sealed trait SyntacticView extends EditorElem {

  /** production names */
  val name: String

  /** parent */
  var parent: Option[SyntacticView] = None

  /** idx of production */
  def idx: Int = this match
    case lex: Lexical               => 0
    case Syntactic(_, _, rhsIdx, _) => rhsIdx
    case abs: AbsSyntactic          => -1

  /** production chains */
  lazy val chains: List[SyntacticView] = this match
    case lex: Lexical => List(this)
    case syn: Syntactic =>
      syn.children.flatten match
        case child :: Nil => this :: child.chains
        case _            => List(this)
    case asy: AbsSyntactic => List(this)

  /** children */
  def getChildren(kind: String): List[SyntacticView] = this match
    case lex: Lexical => List()
    case Syntactic(k, _, _, children) =>
      val founded = (for {
        child <- children.flatten
        found <- child.getChildren(kind)
      } yield found).toList
      if (k == kind) this :: founded else founded
    case abs: AbsSyntactic => List()

  /** types */
  lazy val types: Set[String] =
    Set(name, s"$name$idx") union (this match
      case Syntactic(_, _, _, List(Some(child))) => child.types + "Nonterminal"
      case abs: AbsSyntactic                     => Set("Abstract")
      case _                                     => Set("Terminal")
    )

  def folded: SyntacticView = this match
    case s: Syntactic =>
      s.children match
        case Some(child) :: Nil =>
          val f = child.folded
          f match
            case a: AbsSyntactic =>
              if (a.fold) AbsSyntactic(s.name, a.annotation, true)
              else s.copy(children = List(Some(f)))
            case _ => s.copy(children = List(Some(f)))
        case _ => s.copy(children = s.children.map((vo) => vo.map(_.folded)))
    case _ => this

  def invalidateFold: SyntacticView = this match
    case s: Syntactic =>
      s.copy(children = s.children.map(_.map(_.invalidateFold)))
    case a: AbsSyntactic => a.copy(fold = false)
    case l: Lexical      => this

  def refined(cfgHelper: CFGHelper): SyntacticView = this match
    case s: Syntactic =>
      s.children match
        case Some(child) :: Nil =>
          if (cfgHelper.getSDOView((child, "Evaluation")).isEmpty) this
          else child.refined(cfgHelper)
        case _ => this
    case _ => this
}

sealed trait Annotation
case object AObj extends Annotation {
  override def toString = "AObj"
}
case object ASymbol extends Annotation {
  override def toString = "ASymbol"
}
case object ANum extends Annotation {
  override def toString = "ANum"
}
case object ABigInt extends Annotation {
  override def toString = "ABigInt"
}
case object AStr extends Annotation {
  override def toString = "AStr"
}
case object ABool extends Annotation {
  override def toString = "ABool"
}
case object AUndef extends Annotation {
  override def toString = "AUndef"
}
case object ANull extends Annotation {
  override def toString = "ANull"
}
case object AThrow extends Annotation {
  override def toString = "AThrow"
}
case object AAll extends Annotation {
  override def toString = "AAll"
}

/** ASTs constructed by abstract productions */
case class AbsSyntactic(
  name: String,
  annotation: Annotation = AAll,
  fold: Boolean = false,
) extends SyntacticView

/** ASTs constructed by syntatic productions */
case class Syntactic(
  name: String,
  args: List[Boolean],
  rhsIdx: Int,
  children: List[Option[SyntacticView]],
) extends SyntacticView

/** ASTs constructed by lexical productions */
case class Lexical(
  name: String,
  str: String,
) extends SyntacticView
