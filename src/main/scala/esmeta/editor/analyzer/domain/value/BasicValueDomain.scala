package esmeta.editor.analyzer

import esmeta.util.Appender.*
import esmeta.util.Appender
import esmeta.interp.*
import esmeta.editor.sview.SyntacticView

class BasicValueDomain() extends AbsValueDomain {
  val Bot = Elem(FlatBot, Set(), Set(), true)
  val Top = Elem(FlatTop, ATopClo, ATopClo, false)

  def apply(value: AValue): Elem = value match
    case ALiteral(literal) => Elem(FlatElem(literal), Set.empty, Set.empty)
    case AAst(ast) => Elem(FlatElem(AstValue(ast)), Set.empty, Set.empty, true)
    case ASView(view) => Elem(FlatElem(view), Set.empty, Set.empty, true)
    case AClo(name, captured) =>
      Elem(FlatBot, Set(AClo(name, captured)), Set.empty)
    case ACont(name, captured) =>
      Elem(FlatBot, Set.empty, Set(ACont(name, captured)))
    case _ => Elem(FlatTop, ATopClo, ATopClo)

  def fromAValues[T <: AValue](kind: AValueKind[T])(items: T*): Elem =
    kind match
      case CloKind  => Elem(FlatBot, items.toSet, Set.empty)
      case ContKind => Elem(FlatBot, Set.empty, items.toSet)
      case _        => Elem(FlatTop, Set.empty, Set.empty)

  def mkAbsComp(name: String, value: Elem, target: Elem): Elem =
    (value, target) match {
      case (
            Elem(FlatElem(v: PureValue), _, _, _),
            Elem(FlatElem(Str(target)), _, _, _),
          ) =>
        Elem(FlatElem(Comp(Const(name), v, Some(target))), Set.empty, Set.empty)
      case (
            Elem(FlatElem(v: PureValue), _, _, _),
            Elem(FlatElem(CONST_EMPTY), _, _, _),
          ) =>
        Elem(FlatElem(Comp(Const(name), v, None)), Set.empty, Set.empty)
      case (Elem(FlatBot, _, _, _), _) | (_, Elem(FlatBot, _, _, _)) =>
        Elem(FlatBot, Set.empty, Set.empty)
      case (_, _) => Elem(FlatTop, ATopClo, ATopClo)
    }

  case class Elem(
    fv: Flat[Value | SyntacticView],
    clo: Set[AClo] | ATopClo.type,
    cont: Set[ACont] | ATopClo.type,
    allowTopClo: Boolean = false,
  ) extends AbsValueTrait {

    def removeNormal: Elem = this
    def normal: Elem = this
    def isAbruptCompletion: Elem = this

    def unary_! : Elem = Elem(
      this.fv match
        case FlatTop             => FlatTop
        case (FlatElem(Bool(b))) => FlatElem(Bool(!b))
        case _                   => FlatBot
      ,
      Set(),
      Set(),
    )
    def ||(that: Elem): Elem = Elem(
      (this.fv, that.fv) match {
        case (FlatTop, _) | (_, FlatTop) => FlatTop
        case (FlatElem(Bool(true)), _) | (_, FlatElem(Bool(true))) =>
          FlatElem(Bool(true))
        case (FlatElem(Bool(false)), FlatElem(Bool(false))) =>
          FlatElem(Bool(false))
        case (FlatBot, _) | (_, FlatBot) => FlatBot
        case (_, _)                      => FlatElem(Bool(false))
      },
      Set(),
      Set(),
    )
    def &&(that: Elem): Elem = this

    def =^=(that: Elem): Elem = Elem(
      (this.fv, that.fv) match {
        case (FlatTop, _) | (_, FlatTop) => FlatTop
        case (FlatElem(x), FlatElem(y))  => FlatElem(Bool(x == y))
        case (FlatBot, _) | (_, FlatBot) => FlatBot
      },
      Set(),
      Set(),
    )

    def mul(that: Elem): Elem = this
    def plus(that: Elem): Elem = this
    def min(that: Elem): Elem = this
    def max(that: Elem): Elem = this

    def isCompletion: Elem = this
    def project[T <: AValue](kinds: AValueKind[T]): Elem = this
    def getSingle[T <: AValue](kind: AValueKind[T]): Flat[T] =
      kind match
        case CloKind =>
          clo match
            case ATopClo =>
              if (!isAllowTopClo) throw new Error(s"Exploded $this $kind")
              else FlatTop
            case v: Set[AClo] =>
              if (v.size == 1) FlatElem(v.head)
              else if (v.size == 0) FlatBot
              else FlatTop
        case ContKind =>
          cont match
            case ATopClo =>
              if (!isAllowTopClo) throw new Error(s"Exploded $this $kind")
              else FlatTop
            case v: Set[ACont] =>
              if (v.size == 1) FlatElem(v.head)
              else if (v.size == 0) FlatBot
              else FlatTop
        case _ =>
          fv match
            case FlatTop => FlatTop
            case FlatBot => FlatBot
            case FlatElem(v: Value) =>
              val k = AValue.from(v)
              (kind extractLift k).map(FlatElem(_)).getOrElse(FlatBot)
            case FlatElem(s: SyntacticView) =>
              (kind extractLift (ASView(s))).map(FlatElem(_)).getOrElse(FlatBot)

    def getSet[T <: AValue](kind: AValueKind[T]): Set[T] =
      kind match
        case CloKind =>
          clo match
            case ATopClo =>
              if (!isAllowTopClo) throw new Error(s"Exploded $this $kind")
              else Set()
            case v: Set[AClo] => v
        case ContKind =>
          cont match
            case ATopClo =>
              if (!isAllowTopClo) throw new Error(s"Exploded $this $kind")
              else Set()
            case v: Set[ACont] => v
        case _ =>
          getSingle(
            kind,
          ) match {
            case FlatElem(v) => Set(v)
            case FlatBot     => Set()
            case _           => Set() // unsound only for location
          }
    def escaped: Elem = this
    def wrapCompletion: Elem = this

    def setAllowTopClo(b: Boolean = true): Elem = this.copy(allowTopClo = b)
    def isAllowTopClo = allowTopClo

    def ⊑(that: Elem): Boolean =
      val fve = (this.fv, that.fv) match {
        case (FlatBot, _)                 => true
        case (_, FlatTop)                 => true
        case (FlatElem(v1), FlatElem(v2)) => v1 == v2
        case (_, _)                       => false
      }
      val cloe = (this.clo, that.clo) match {
        case (_, ATopClo) => true
        case (ATopClo, _) => false
        case (s1: Set[AClo], s2: Set[AClo]) =>
          (s1.map(_.func.name) ++ s2.map(_.func.name)).forall {
            case key =>
              (s1.find(_.func.name == key), s2.find(_.func.name == key)) match {
                case (None, _) => true
                case (Some(p1), Some(p2)) =>
                  (p1.captured.keySet ++ p2.captured.keySet).forall {
                    case name =>
                      (p1.captured.get(name), p2.captured.get(name)) match {
                        case (None, _)            => true
                        case (Some(a1), Some(a2)) => a1 ⊑ a2
                        case (_, None)            => false
                      }
                  }
                case (_, None) => false
              }
          }
      }
      val conte = (this.cont, that.cont) match {
        case (_, ATopClo) => true
        case (ATopClo, _) => false
        case (s1: Set[ACont], s2: Set[ACont]) =>
          (s1.map(_.func.name) ++ s2.map(_.func.name)).forall {
            case key =>
              (s1.find(_.func.name == key), s2.find(_.func.name == key)) match {
                case (None, _) => true
                case (Some(p1), Some(p2)) =>
                  (p1.captured.keySet ++ p2.captured.keySet).forall {
                    case name =>
                      (p1.captured.get(name), p2.captured.get(name)) match {
                        case (None, _)            => true
                        case (Some(a1), Some(a2)) => a1 ⊑ a2
                        case (_, None)            => false
                      }
                  }
                case (_, None) => false
              }
          }
      }
      val allowj = (this.allowTopClo, that.allowTopClo) match {
        case (false, true) => false
        case _             => true
      }
      fve && cloe && conte && allowj

    // join operator
    def ⊔(that: Elem): Elem =
      val fvj = (this.fv, that.fv) match {
        case (_, FlatBot) | (FlatTop, _)              => this.fv
        case (FlatBot, _) | (_, FlatTop)              => that.fv
        case (FlatElem(v1), FlatElem(v2)) if v1 == v2 => this.fv
        case (FlatElem(_), FlatElem(_))               => FlatTop
      }
      val cloj: Set[AClo] | ATopClo.type = (this.clo, that.clo) match {
        case (_, ATopClo) | (ATopClo, _) => ATopClo
        case (s1: Set[AClo], s2: Set[AClo]) =>
          (s1.map(_.func.name) ++ s2.map(_.func.name)).map {
            case key =>
              (s1.find(_.func.name == key), s2.find(_.func.name == key)) match {
                case (None, Some(p2)) => p2
                case (Some(p1), Some(p2)) =>
                  AClo(
                    p1.func,
                    (p1.captured.keySet ++ p2.captured.keySet).map {
                      case name =>
                        name -> ((
                          p1.captured.get(name),
                          p2.captured.get(name),
                        ) match {
                          case (None, Some(x))      => x
                          case (Some(a1), Some(a2)) => a1 ⊔ a2
                          case (Some(x), None)      => x
                          case (None, None)         => throw Error("A")
                        })
                    }.toMap,
                  )
                case (Some(p1), None) => p1
                case (None, None)     => throw Error("A")
              }
          }
      }
      val contj: Set[ACont] | ATopClo.type = (this.cont, that.cont) match {
        case (_, ATopClo) | (ATopClo, _) => ATopClo
        case (s1: Set[ACont], s2: Set[ACont]) =>
          (s1.map(_.func.name) ++ s2.map(_.func.name)).map {
            case key =>
              (s1.find(_.func.name == key), s2.find(_.func.name == key)) match {
                case (None, Some(p2)) => p2
                case (Some(p1), Some(p2)) =>
                  ACont(
                    p1.func,
                    (p1.captured.keySet ++ p2.captured.keySet).map {
                      case name =>
                        name -> ((
                          p1.captured.get(name),
                          p2.captured.get(name),
                        ) match {
                          case (None, Some(x))      => x
                          case (Some(a1), Some(a2)) => a1 ⊔ a2
                          case (Some(x), None)      => x
                          case (None, None)         => throw Error("A")
                        })
                    }.toMap,
                  )
                case (Some(p1), None) => p1
                case (None, None)     => throw Error("A")
              }
          }
      }
      val allowj = (this.allowTopClo, that.allowTopClo) match {
        case (true, true) => true
        case _            => false
      }
      Elem(fvj, cloj, contj, allowj)

    // conversion to string
    def toString(detail: Boolean): String = {
      val app = new Appender
      app >> this
      app.toString
    }

    override def toString(grammar: Option[esmeta.spec.Grammar]): String = {
      fv match {
        case FlatTop                    => s"⊤ ${this.allowTopClo}"
        case FlatBot                    => "⊥"
        case FlatElem(s: SyntacticView) => s.toString(true, false, grammar)
        case FlatElem(v)                => v.toString
      }
    }
  }

  // appender
  implicit val app: Rule[Elem] = (app, elem) =>
    app >> {
      elem.fv match {
        case FlatTop     => "⊤"
        case FlatBot     => "⊥"
        case FlatElem(v) => v.toString
      }
    } >> ""

}
