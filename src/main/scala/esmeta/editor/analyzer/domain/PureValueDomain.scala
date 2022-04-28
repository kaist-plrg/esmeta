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

class PureValueDomain extends Domain:
  val Bot = EBot

  val Top = EKind(
    Set(
      ValueKind.Undef,
      ValueKind.Null,
      ValueKind.Bool,
      ValueKind.Str,
      ValueKind.Num,
      ValueKind.BigInt,
      ValueKind.Obj,
      ValueKind.Ast,
      ValueKind.List,
      ValueKind.Record,
      ValueKind.Absent,
      ValueKind.Etc,
    ),
  )
  val TopOpt = Some(Top)
  enum ValueKind:
    case Undef, Null, Bool, Str, Symbol, Num, BigInt, Obj, Ast, List, Record,
    Absent, Etc
  end ValueKind

  sealed trait Elem extends ElemTrait:

    override def beautify(grammar: Option[esmeta.spec.Grammar]) =
      this match
        case EKind(s)                => s"⊤ (${s})"
        case EBot                    => "⊥"
        case EFlat(s: SyntacticView) => s.toString(true, false, grammar)
        case EFlat(v)                => v.toString

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
  end Elem

  case object EBot extends Elem
  case class EFlat(
    v: AstValue | Grammar | LiteralValue | SyntacticView | AllocSite,
  ) extends Elem:
    def kind = v match
      case _: AstValue                         => ValueKind.Ast
      case _: Str                              => ValueKind.Str
      case _: SyntacticView                    => ValueKind.Ast
      case _: Math | _: Number                 => ValueKind.Num
      case _: BigInt                           => ValueKind.BigInt
      case _: Bool                             => ValueKind.Bool
      case Undef                               => ValueKind.Undef
      case Null                                => ValueKind.Null
      case Absent                              => ValueKind.Absent
      case ListAllocSite                       => ValueKind.List
      case _: RecordAllocSite                  => ValueKind.Record
      case SymbolAllocSite                     => ValueKind.Symbol
      case _: ObjAllocSite                     => ValueKind.Obj
      case _: Grammar | _: Const | _: CodeUnit => ValueKind.Etc
  end EFlat
  case class EKind(s: Set[ValueKind]) extends Elem

  // appender
  implicit val app: Rule[Elem] = (app, elem) => app >> elem.beautify(None)
end PureValueDomain
