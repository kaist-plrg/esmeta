package esmeta.editor.analyzer

import esmeta.util.Appender.*
import esmeta.util.Appender
import esmeta.interp.*
import esmeta.editor.sview.SyntacticView

class BasicValueDomain() extends AbsValueDomain {
  val Bot = Elem(FlatBot)
  val Top = Elem(FlatTop)

  def apply(value: AValue): Elem = value match
    case ALiteral(literal)    => Elem(FlatElem(literal))
    case AAst(ast)            => Elem(FlatElem(AstValue(ast)))
    case ASView(view)         => Elem(FlatElem(view))
    case AClo(name, captured) => Elem(FlatElem(AClo(name, captured)))
    case ACont(name, captured) =>
      Elem(FlatElem(ACont(name, captured)))
    case _ => Elem(FlatTop)

  def fromAValues[T <: AValue](kind: AValueKind[T])(items: T*): Elem = Elem(
    FlatTop,
  )
  def mkAbsComp(name: String, value: Elem, target: Elem): Elem =
    (value, target) match {
      case (Elem(FlatElem(v: PureValue)), Elem(FlatElem(Str(target)))) =>
        Elem(FlatElem(Comp(Const(name), v, Some(target))))
      case (Elem(FlatElem(v: PureValue)), Elem(FlatElem(CONST_EMPTY))) =>
        Elem(FlatElem(Comp(Const(name), v, None)))
      case (Elem(FlatBot), _) | (_, Elem(FlatBot)) => Elem(FlatBot)
      case (_, _)                                  => Elem(FlatTop)
    }

  case class Elem(f: Flat[Value | AClo | ACont | SyntacticView])
    extends AbsValueTrait {

    def removeNormal: Elem = this
    def normal: Elem = this
    def isAbruptCompletion: Elem = this

    def unary_! : Elem = this
    def ||(that: Elem): Elem = Elem((this.f, that.f) match {
      case (FlatTop, _) | (_, FlatTop) => FlatTop
      case (FlatElem(Bool(true)), _) | (_, FlatElem(Bool(true))) =>
        FlatElem(Bool(true))
      case (FlatElem(Bool(false)), FlatElem(Bool(false))) =>
        FlatElem(Bool(false))
      case (FlatBot, _) | (_, FlatBot) => FlatBot
      case (_, _)                      => FlatElem(Bool(false))
    })
    def &&(that: Elem): Elem = this

    def =^=(that: Elem): Elem = Elem((this.f, that.f) match {
      case (FlatTop, _) | (_, FlatTop) => FlatTop
      case (FlatElem(x), FlatElem(y))  => FlatElem(Bool(x == y))
      case (FlatBot, _) | (_, FlatBot) => FlatBot
    })

    def mul(that: Elem): Elem = this
    def plus(that: Elem): Elem = this
    def min(that: Elem): Elem = this
    def max(that: Elem): Elem = this

    def isCompletion: Elem = this
    def project(kinds: AValueKind[AValue]*): Elem = this
    def getSingle[T <: AValue](kind: AValueKind[T]): Flat[T] =
      f match
        case FlatTop => FlatTop
        case FlatBot => FlatBot
        case FlatElem(v: Value) =>
          val k = AValue.from(v)
          (kind extractLift k).map(FlatElem(_)).getOrElse(FlatBot)
        case FlatElem(s: SyntacticView) =>
          (kind extractLift (ASView(s))).map(FlatElem(_)).getOrElse(FlatBot)
        case FlatElem(v: AClo) =>
          (kind extractLift v).map(FlatElem(_)).getOrElse(FlatBot)
        case FlatElem(v: ACont) =>
          (kind extractLift v).map(FlatElem(_)).getOrElse(FlatBot)

    def getSet[T <: AValue](kind: AValueKind[T]): Set[T] = getSingle(
      kind,
    ) match {
      case FlatElem(v) => Set(v)
      case _           => Set()
    }
    def escaped: Elem = this
    def wrapCompletion: Elem = this

    def ⊑(that: Elem): Boolean = (this.f, that.f) match {
      case (FlatBot, _)                 => true
      case (_, FlatTop)                 => true
      case (FlatElem(v1), FlatElem(v2)) => v1 == v2
      case (_, _)                       => false
    }

    // join operator
    def ⊔(that: Elem): Elem = (this.f, that.f) match {
      case (_, FlatBot)                             => this
      case (FlatBot, _)                             => that
      case (FlatTop, _) | (_, FlatTop)              => that
      case (FlatElem(v1), FlatElem(v2)) if v1 == v2 => this
      case (FlatElem(_), FlatElem(_))               => Elem(FlatTop)
    }

    // conversion to string
    def toString(detail: Boolean): String = {
      val app = new Appender
      app >> this
      app.toString
    }
  }

  // appender
  implicit val app: Rule[Elem] = (app, elem) =>
    app >> {
      elem.f match {
        case FlatTop     => "⊤"
        case FlatBot     => "⊥"
        case FlatElem(v) => v.toString
      }
    } >> ""

}
