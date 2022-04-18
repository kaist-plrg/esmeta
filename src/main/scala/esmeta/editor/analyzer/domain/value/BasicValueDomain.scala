package esmeta.editor.analyzer

import esmeta.util.Appender.*
import esmeta.util.Appender
import esmeta.interp.*
import esmeta.editor.sview.SyntacticView

class BasicValueDomain() extends AbsValueDomain {

  val purd = PureValueDomain()
  val clod = ACloDomain(this)
  val contd = AContDomain(this)
  val Bot = Elem(purd.Bot, clod.Bot, contd.Bot)
  val Top = Elem(purd.Top, clod.ETopClo, contd.ETopCont)

  val banMethods: Set[String] =
    Set(
      "GetValue",
    ) // , "Get", "GetV", "GetMethod", "Call", "OrdinaryToPrimitive")

  def apply(value: AValue): Elem = value match
    case ALiteral(literal) => Elem(purd.EFlat(literal), clod.Bot, contd.Bot)
    case AAst(ast)    => Elem(purd.EFlat(AstValue(ast)), clod.Bot, contd.Bot)
    case ASView(view) => Elem(purd.EFlat(view), clod.Bot, contd.Bot)
    case AClo(func, _) if banMethods contains func.name =>
      Elem(purd.Bot, clod.EIgnoreClo, contd.Bot)
    case AClo(func, captured) =>
      Elem(purd.Bot, clod.ESet(Set(AClo(func, captured))), contd.Bot)
    case ACont(func, captured) =>
      Elem(purd.Bot, clod.Bot, contd.ESet(Set(ACont(func, captured))))
    case _ => Elem(purd.Top, clod.ETopClo, contd.ETopCont)

  def fromAValues[T <: AValue](kind: AValueKind[T])(items: T*): Elem =
    kind match
      case CloKind  => Elem(purd.Bot, clod.ESet(items.toSet), contd.Bot)
      case ContKind => Elem(purd.Bot, clod.Bot, contd.ESet(items.toSet))
      case _        => Elem(purd.Top, clod.Bot, contd.Bot)

  def mkAbsComp(name: String, value: Elem, target: Elem): Elem =
    (value, target) match {
      case (
            Elem(purd.EFlat(v: PureValue), _, _),
            Elem(purd.EFlat(Str(target)), _, _),
          ) =>
        Elem(
          purd.EFlat(Comp(Const(name), v, Some(target))),
          clod.Bot,
          contd.Bot,
        )
      case (
            Elem(purd.EFlat(v: PureValue), _, _),
            Elem(purd.EFlat(CONST_EMPTY), _, _),
          ) =>
        Elem(purd.EFlat(Comp(Const(name), v, None)), clod.Bot, contd.Bot)
      case (Elem(purd.Bot, _, _), _) | (_, Elem(purd.Bot, _, _)) =>
        Bot
      case (_, _) => Top
    }

  // Set(ASTType | NumType | BoolType | AddType | EtcType) * (CloType)             * (ContType)
  // (Value | SyntacticView)                               * (Set[AClo] | ATopClo) * (Set[ACont] | ATopClo)
  // Bot

  case class Elem(
    pure: purd.Elem,
    clo: clod.Elem,
    cont: contd.Elem,
  ) extends AbsValueTrait {

    def removeNormal: Elem = this
    def normal: Elem = this
    def isAbruptCompletion: Elem = this

    def unary_! : Elem = Elem(
      pure match
        case purd.EKind(_)         => purd.EKind(Set(purd.ValueKind.Bool))
        case (purd.EFlat(Bool(b))) => purd.EFlat(Bool(!b))
        case _                     => purd.Bot
      ,
      clod.Bot,
      contd.Bot,
    )
    def ||(that: Elem): Elem = Elem(
      (this.pure, that.pure) match {
        case (purd.EKind(_), _) | (_, purd.EKind(_)) =>
          purd.EKind(Set(purd.ValueKind.Bool))
        case (purd.EFlat(Bool(true)), _) | (_, purd.EFlat(Bool(true))) =>
          purd.EFlat(Bool(true))
        case (purd.EFlat(Bool(false)), purd.EFlat(Bool(false))) =>
          purd.EFlat(Bool(false))
        case (purd.EBot, _) | (_, purd.EBot) => purd.EBot
        case (_, _)                          => purd.EFlat(Bool(false))
      },
      clod.Bot,
      contd.Bot,
    )
    def &&(that: Elem): Elem = this

    def =^=(that: Elem): Elem = Elem(
      (this.pure, that.pure) match {
        case (purd.EKind(_), _) | (_, purd.EKind(_)) =>
          purd.EKind(Set(purd.ValueKind.Bool))
        case (purd.EFlat(x), purd.EFlat(y))  => purd.EFlat(Bool(x == y))
        case (purd.EBot, _) | (_, purd.EBot) => purd.EBot
      },
      clod.Bot,
      contd.Bot,
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
            case clod.ETopClo =>
              throw new Error(s"Exploded $this $kind")
            case clod.EIgnoreClo =>
              FlatTop
            case clod.ESet(v: Set[AClo]) =>
              if (v.size == 1) FlatElem(v.head)
              else if (v.size == 0) FlatBot
              else FlatTop
        case ContKind =>
          cont match
            case contd.ETopCont =>
              throw new Error(s"Exploded $this $kind")
            case contd.EIgnoreCont =>
              FlatTop
            case contd.ESet(v: Set[ACont]) =>
              if (v.size == 1) FlatElem(v.head)
              else if (v.size == 0) FlatBot
              else FlatTop
        case _ =>
          pure match
            case purd.EKind(_) => FlatTop
            case purd.EBot     => FlatBot
            case purd.EFlat(v: (AstValue | Comp | Grammar | LiteralValue)) =>
              val k = AValue.from(v)
              (kind extractLift k).map(FlatElem(_)).getOrElse(FlatBot)
            case purd.EFlat(s: SyntacticView) =>
              (kind extractLift (ASView(s))).map(FlatElem(_)).getOrElse(FlatBot)

    def getSet[T <: AValue](kind: AValueKind[T]): Set[T] =
      kind match
        case CloKind =>
          clo match
            case clod.ETopClo =>
              throw new Error(s"Exploded $this $kind")
            case clod.EIgnoreClo =>
              Set()
            case clod.ESet(v: Set[AClo]) => v
        case ContKind =>
          cont match
            case contd.ETopCont =>
              throw new Error(s"Exploded $this $kind")
            case contd.EIgnoreCont =>
              Set()
            case contd.ESet(v: Set[ACont]) => v
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

    def setAllowTopClo(b: Boolean = true): Elem = if (b)
      this.copy(
        clo = (if (clo == clod.ETopClo) clod.EIgnoreClo else clo),
        cont = (if (cont == contd.ETopCont) contd.EIgnoreCont else cont),
      )
    else
      this.copy(
        clo = (if (clo == clod.EIgnoreClo) clod.ETopClo else clo),
        cont = (if (cont == contd.EIgnoreCont) contd.ETopCont else cont),
      )
    def isAllowTopClo =
      this.clo ⊑ clod.EIgnoreClo && this.cont ⊑ contd.EIgnoreCont

    def ⊑(that: Elem): Boolean =
      this.pure ⊑ that.pure && this.clo ⊑ that.clo && this.cont ⊑ that.cont

    // join operator
    def ⊔(that: Elem): Elem =
      Elem(this.pure ⊔ that.pure, this.clo ⊔ that.clo, this.cont ⊔ that.cont)

    // conversion to string
    def toString(detail: Boolean): String = {
      val app = new Appender
      app >> this
      app.toString
    }

    override def toString(grammar: Option[esmeta.spec.Grammar]): String = {
      pure match {
        case purd.EKind(s)                => s"⊤ ${s} ${this.isAllowTopClo}"
        case purd.EBot                    => "⊥"
        case purd.EFlat(s: SyntacticView) => s.toString(true, false, grammar)
        case purd.EFlat(v)                => v.toString
      }
    }
  }

  // appender
  implicit val app: Rule[Elem] = (app, elem) =>
    app >> {
      elem.pure match {
        case purd.EKind(_) => "⊤"
        case purd.EBot     => "⊥"
        case purd.EFlat(v) => v.toString
      }
    } >> ""

}
