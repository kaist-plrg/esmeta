package esmeta.editor.sview

import esmeta.js.Ast
import esmeta.editor.*

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
}

sealed trait Annotation
case object AObj extends Annotation
case object ASymbol extends Annotation
case object ANum extends Annotation
case object ABigInt extends Annotation
case object AStr extends Annotation
case object ABool extends Annotation
case object AUndef extends Annotation
case object ANull extends Annotation
case object AThrow extends Annotation
case object AAll extends Annotation

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
