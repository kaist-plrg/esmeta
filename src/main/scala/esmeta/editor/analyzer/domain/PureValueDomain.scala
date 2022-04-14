package esmeta.editor.analyzer

import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*
import esmeta.editor.sview.SyntacticView
import esmeta.interp.{
  AstValue,
  Grammar,
  Comp,
  LiteralValue,
  Str,
  Math,
  Number,
  BigInt,
  Bool,
  Undef,
  Null,
  Absent,
  CodeUnit,
  Const,
}

// Set(ASTType | NumType | BoolType | StrType | AddrType | EtcType)
// (AstValue | Grammar | LiteralValue | SyntacticView)
// Bot

class PureValueDomain extends Domain {
  val Bot = EBot

  val Top = EKind(
    Set(
      ValueKind.Ast,
      ValueKind.Num,
      ValueKind.Bool,
      ValueKind.Str,
      ValueKind.Addr,
      ValueKind.Comp,
      ValueKind.Undef,
      ValueKind.Null,
      ValueKind.Etc,
    ),
  )
  enum ValueKind:
    case Ast, Num, Bool, Str, Addr, Comp, Undef, Null, Etc

  sealed trait Elem extends ElemTrait {
    def ⊑(that: Elem): Boolean = (this, that) match
      case (EBot, _)              => true
      case (_, EBot)              => false
      case (a: EFlat, b: EFlat)   => a == b
      case (a: EFlat, EKind(s))   => s contains a.kind
      case (EKind(_), EFlat(_))   => false
      case (EKind(s1), EKind(s2)) => s1.forall(s2.contains(_))
    def ⊔(that: Elem): Elem = (this, that) match
      case (EBot, _) => that
      case (_, EBot) => this
      case (a: EFlat, b: EFlat) =>
        if (a == b) then this else EKind(Set(a.kind, b.kind))
      case (a: EFlat, EKind(s))   => EKind(s + a.kind)
      case (EKind(s), a: EFlat)   => EKind(s + a.kind)
      case (EKind(s1), EKind(s2)) => EKind(s1 ++ s2)
  }
  case object EBot extends Elem
  case class EFlat(v: AstValue | Comp | Grammar | LiteralValue | SyntacticView)
    extends Elem {
    def kind = v match
      case _: AstValue                                  => ValueKind.Ast
      case _: Comp                                      => ValueKind.Comp
      case _: Str                                       => ValueKind.Str
      case _: SyntacticView                             => ValueKind.Ast
      case _: Math | _: BigInt | _: Number              => ValueKind.Num
      case _: Bool                                      => ValueKind.Bool
      case Undef                                        => ValueKind.Undef
      case Null                                         => ValueKind.Null
      case _: Grammar | _: Const | Absent | _: CodeUnit => ValueKind.Etc
  }
  case class EKind(s: Set[ValueKind]) extends Elem

  // appender
  implicit val app: Rule[Elem] = (app, elem) => app >> elem.toString

}
