package esmeta.editor.analyzer

import esmeta.util.Appender.*
import esmeta.util.Appender
import esmeta.interp.*
import esmeta.editor.sview.SyntacticView

class BasicValueDomain() extends AbsValueDomain {

  enum CompType:
    case Empty, N, A
  end CompType

  val purd = PureValueDomain()
  val clod = ACloDomain(this)
  val contd = AContDomain(this)
  val compd = SetDomain(List(CompType.Empty, CompType.N, CompType.A))

  def pureKind(v: purd.ValueKind): Elem = pureElem(purd.EKind(Set(v)))
  def pureElem(v: purd.Elem): Elem =
    Elem(v, clod.Bot, contd.Bot, compd.Base(Set(CompType.Empty)))
  def clo(s: Set[AClo]): Elem =
    Elem(purd.Bot, clod.ESet(s), contd.Bot, compd.Base(Set(CompType.Empty)))
  def cont(s: Set[ACont]): Elem =
    Elem(purd.Bot, clod.Bot, contd.ESet(s), compd.Base(Set(CompType.Empty)))

  val Bot = Elem(purd.Bot, clod.Bot, contd.Bot, compd.Bot)
  val Top = Elem(purd.Top, clod.ETopClo, contd.ETopCont, compd.Top)

  val handlerMethods: Map[String, (List[Elem] => Elem)] =
    Map(
      "GetValue" -> (ls => ls(0)),
      "ANum" -> ((ls: List[Elem]) => pureKind(purd.ValueKind.Num)),
      "AStr" -> ((ls: List[Elem]) => pureKind(purd.ValueKind.Str)),
      "ABool" -> ((ls: List[Elem]) => pureKind(purd.ValueKind.Bool)),
    )
  // , "Get", "GetV", "GetMethod", "Call", "OrdinaryToPrimitive")

  def findHandler(s: String): Elem = handlerMethods
    .get(s)
    .map((f) =>
      Elem(
        purd.Bot,
        clod.EHandler(s, f),
        contd.Bot,
        compd.Base(Set(CompType.Empty)),
      ),
    )
    .getOrElse(Top)

  def apply(value: AValue): Elem = value match
    case ALiteral(literal) =>
      Elem(
        purd.EFlat(literal),
        clod.Bot,
        contd.Bot,
        compd.Base(Set(CompType.Empty)),
      )
    case AAst(ast) =>
      Elem(
        purd.EFlat(AstValue(ast)),
        clod.Bot,
        contd.Bot,
        compd.Base(Set(CompType.Empty)),
      )
    case ASView(view) =>
      Elem(
        purd.EFlat(view),
        clod.Bot,
        contd.Bot,
        compd.Base(Set(CompType.Empty)),
      )
    case AClo(func, _) if handlerMethods contains func.name =>
      Elem(
        purd.Bot,
        clod.EHandler(func.name, handlerMethods(func.name)),
        contd.Bot,
        compd.Base(Set(CompType.Empty)),
      )
    case AClo(func, captured) =>
      Elem(
        purd.Bot,
        clod.ESet(Set(AClo(func, captured))),
        contd.Bot,
        compd.Base(Set(CompType.Empty)),
      )
    case ACont(func, captured) =>
      Elem(
        purd.Bot,
        clod.Bot,
        contd.ESet(Set(ACont(func, captured))),
        compd.Base(Set(CompType.Empty)),
      )
    case _ =>
      Elem(purd.Top, clod.Bot, contd.Bot, compd.Base(Set(CompType.Empty)))

  def fromAValues[T <: AValue](kind: AValueKind[T])(items: T*): Elem =
    kind match
      case CloKind =>
        Elem(
          purd.Bot,
          clod.ESet(items.toSet),
          contd.Bot,
          compd.Base(Set(CompType.Empty)),
        )
      case ContKind =>
        Elem(
          purd.Bot,
          clod.Bot,
          contd.ESet(items.toSet),
          compd.Base(Set(CompType.Empty)),
        )
      case StrKind =>
        if (items.length == 1)
          Elem(purd.EFlat(items.head.simple), clod.Bot, contd.Bot, compd.Bot)
        else
          Elem(
            purd.EKind(Set(purd.ValueKind.Str)),
            clod.Bot,
            contd.Bot,
            compd.Base(Set(CompType.Empty)),
          )
      case CompKind => Elem(purd.Top, clod.Bot, contd.Bot, compd.Top)
      case _ =>
        Elem(purd.Top, clod.Bot, contd.Bot, compd.Base(Set(CompType.Empty)))

  def mkAbsComp(name: String, value: Elem, target: Elem): Elem =
    (value, target) match {
      case (
            Elem(purd.EFlat(v: PureValue), _, _, _),
            Elem(purd.EFlat(Str(target)), _, _, _),
          ) =>
        Elem(
          value.pure,
          clod.Bot,
          contd.Bot,
          compd.Top,
        )
      case (
            Elem(purd.EFlat(v: PureValue), _, _, _),
            Elem(purd.EFlat(CONST_EMPTY), _, _, _),
          ) =>
        Elem(value.pure, clod.Bot, contd.Bot, compd.Top)
      case (Elem(purd.Bot, _, _, _), _) | (_, Elem(purd.Bot, _, _, _)) =>
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
    compt: compd.Elem,
  ) extends AbsValueTrait {

    def removeNormal: Elem = this
    def normal: Elem = this
    def isAbruptCompletion: Elem = this

    def getHandler = clo match {
      case clod.EHandler(_, f) => Some(f)
      case _                   => None
    }

    def unary_! : Elem = Elem(
      pure match
        case purd.EKind(_)         => purd.EKind(Set(purd.ValueKind.Bool))
        case (purd.EFlat(Bool(b))) => purd.EFlat(Bool(!b))
        case _                     => purd.Bot
      ,
      clod.Bot,
      contd.Bot,
      compd.Base(Set(CompType.Empty)),
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
      compd.Base(Set(CompType.Empty)),
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
      compd.Base(Set(CompType.Empty)),
    )

    def mul(that: Elem): Elem = this
    def plus(that: Elem): Elem = this
    def min(that: Elem): Elem = this
    def max(that: Elem): Elem = this

    def isCompletion: Elem = Top
    /*
      if (this.compt.contains(CompType.A) || this.compt.contains(CompType.N))
        if (this.compt.contains(CompType.Empty)) Elem(purd.EKind(Set(purd.ValueKind.Bool)), clod.Bot, contd.Bot, compd.Base(Set(CompType.Empty)))
        else Elem(purd.EFlat(Bool(true)), clod.Bot, contd.Bot, compd.Base(Set(CompType.Empty)))
      else
        if
      case purd.EBot => Elem(purd.EFlat(Bool(false)), clod.Bot, contd.Bot, compd.Bot)
      case f: purd.EFlat => f.kind match
        case purd.ValueKind.NComp | purd.ValueKind.AComp => Elem(purd.EFlat(Bool(true)), clod.Bot, contd.Bot, compd.Bot)
        case _ => Elem(purd.EFlat(Bool(false)), clod.Bot, contd.Bot, compd.Bot)
      case purd.EKind(s) =>
        if ((s contains purd.ValueKind.NComp) || (s contains purd.ValueKind.AComp))
          if ((s - purd.ValueKind.NComp - purd.ValueKind.AComp).isEmpty) Elem(purd.EFlat(Bool(true)), clod.Bot, contd.Bot, compd.Bot)
          else Elem(purd.EKind(Set(purd.ValueKind.Bool)), clod.Bot, contd.Bot, compd.Bot)
        else Elem(purd.EFlat(Bool(true)), clod.Bot, contd.Bot, compd.Bot)
     */
    def projValueKind(v: purd.ValueKind) = pure match {
      case f: purd.EFlat =>
        if (f.kind == v)
          this.copy(
            pure = f,
            cont = contd.Bot,
            clo = clod.Bot,
            compt = compd.Base(Set(CompType.Empty)),
          )
        else Bot
      case s: purd.EKind =>
        if (s.s contains v)
          this.copy(
            pure = purd.EKind(Set(v)),
            cont = contd.Bot,
            clo = clod.Bot,
            compt = compd.Base(Set(CompType.Empty)),
          )
        else Bot
      case purd.EBot => Bot
    }

    def project[T <: AValue](kinds: AValueKind[T]): Elem =
      kinds match
        case CloKind    => this.copy(pure = purd.Bot, cont = contd.Bot)
        case ContKind   => this.copy(pure = purd.Bot, clo = clod.Bot)
        case CompKind   => this
        case LocKind    => projValueKind(purd.ValueKind.Addr)
        case StrKind    => projValueKind(purd.ValueKind.Str)
        case NumKind    => projValueKind(purd.ValueKind.Num)
        case AllKind    => this
        case BoolKind   => projValueKind(purd.ValueKind.Bool)
        case BigIntKind => projValueKind(purd.ValueKind.Etc)
        case NullKind   => projValueKind(purd.ValueKind.Null)
        case UndefKind  => projValueKind(purd.ValueKind.Undef)
        case _          => this

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
            case clod.EHandler(_, _) => FlatTop
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
            case clod.EHandler(_, _)     => Set()
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
      Elem(
        this.pure ⊔ that.pure,
        this.clo ⊔ that.clo,
        this.cont ⊔ that.cont,
        this.compt ⊔ that.compt,
      )

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
