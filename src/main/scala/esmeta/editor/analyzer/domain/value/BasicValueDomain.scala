package esmeta.editor.analyzer

import esmeta.util.Appender.*
import esmeta.util.Appender
import esmeta.interp.*
import esmeta.editor.sview.SyntacticView

class BasicValueDomain() extends AbsValueDomain {

  val purd = PureValueDomain()
  val clod = ACloDomain(this)
  val contd = AContDomain(this)
  val compd = CompDomain[purd.type](purd)

  def pureKind(v: purd.ValueKind): Elem = pureElem(purd.EKind(Set(v)))
  def pureElem(v: purd.Elem): Elem =
    Elem(pure = v)
  def cont(s: Set[ACont]): Elem =
    Elem(cont = contd.ESet(s))

  val Bot = Elem()
  val Top = Elem(purd.Top, clod.ETopClo, contd.ETopCont, compd.Top)

  val handlerMethods: Map[String, (List[Elem] => Elem)] =
    Map(
      "GetValue" -> (ls => ls(0)),
      "AAll" -> ((ls: List[Elem]) =>
        Elem(
          pure = purd.EKind(
            Set(
              purd.ValueKind.Obj,
              purd.ValueKind.Symbol,
              purd.ValueKind.Num,
              purd.ValueKind.BigInt,
              purd.ValueKind.Str,
              purd.ValueKind.Bool,
              purd.ValueKind.Undef,
              purd.ValueKind.Null,
            ),
          ),
          compt = compd.EBase(
            compt = compd.typed(compd.CompType.A),
            pure = purd.Top,
            target = purd.Top,
          ),
        ),
      ),
      "AObj" -> ((ls: List[Elem]) => pureKind(purd.ValueKind.Obj)),
      "ASymbol" -> ((ls: List[Elem]) => pureKind(purd.ValueKind.Symbol)),
      "ANum" -> ((ls: List[Elem]) => pureKind(purd.ValueKind.Num)),
      "ABigInt" -> ((ls: List[Elem]) => pureKind(purd.ValueKind.BigInt)),
      "AStr" -> ((ls: List[Elem]) => pureKind(purd.ValueKind.Str)),
      "ABool" -> ((ls: List[Elem]) => pureKind(purd.ValueKind.Bool)),
      "AUndef" -> ((ls: List[Elem]) => pureKind(purd.ValueKind.Undef)),
      "ANull" -> ((ls: List[Elem]) => pureKind(purd.ValueKind.Null)),
      "AThrow" -> ((ls: List[Elem]) =>
        Elem(compt =
          compd.EBase(
            compt = compd.typed(compd.CompType.A),
            pure = purd.Top,
            target = purd.Top,
          ),
        ),
      ),
    )
  // , "Get", "GetV", "GetMethod", "Call", "OrdinaryToPrimitive")

  def findHandler(name: String, kind: String): Elem = handlerMethods
    .get(kind)
    .map((f) =>
      Elem(
        clo = clod.EHandler(name, kind, f),
      ),
    )
    .getOrElse(Top)

  def apply(value: AValue): Elem = value match
    case ALiteral(literal) =>
      Elem(
        pure = purd.EFlat(literal),
      )
    case AAst(ast) =>
      Elem(
        pure = purd.EFlat(AstValue(ast)),
      )
    case ASView(view) =>
      Elem(
        pure = purd.EFlat(view),
      )
    case AClo(func, _) if handlerMethods contains func.name =>
      Elem(
        clo = clod.EHandler(func.name, func.name, handlerMethods(func.name)),
      )
    case AClo(func, captured) =>
      Elem(
        clo = clod.ESet(Set((AClo(func, captured), Map()))),
      )
    case ACont(func, captured) =>
      Elem(
        cont = contd.ESet(Set(ACont(func, captured))),
      )
    case AComp(ALiteral(Const(s)), v, t) => Elem(compt = compd.Top)
    case Loc(ObjAllocSite(ty)) => Elem(pure = purd.EFlat(ObjAllocSite(ty)))
    case Loc(RecordAllocSite(ty)) =>
      Elem(pure = purd.EFlat(RecordAllocSite(ty)))
    case Loc(ListAllocSite)   => Elem(pure = purd.EFlat(ListAllocSite))
    case Loc(SymbolAllocSite) => Elem(pure = purd.EFlat(SymbolAllocSite))
    case _ =>
      Elem(pure = purd.Top)

  def fromBoundedAClos(items: (AClo, Map[Int, Elem])*): Elem =
    Elem(clo = clod.ESet(items.toSet))
  def fromAValues[T <: AValue](kind: AValueKind[T])(items: T*): Elem =
    kind match
      case CloKind =>
        Elem(
          clo = clod.ESet(items.map((s) => (s, Map())).toSet),
        )
      case ContKind =>
        Elem(
          cont = contd.ESet(items.toSet),
        )
      case StrKind =>
        if (items.length == 1)
          Elem(pure = purd.EFlat(items.head.simple))
        else
          Elem(
            pure = purd.EKind(Set(purd.ValueKind.Str)),
          )
      case CompKind => Elem(compt = compd.Top)
      case _ =>
        Elem(pure = purd.Top)

  def mkAbsComp(name: String, value: Elem, target: Elem): Elem =
    (value, target) match {
      case (
            Elem(purd.EFlat(v: PureValue), _, _, _),
            Elem(purd.EFlat(Str(target)), _, _, _),
          ) =>
        Elem(
          compt = compd.EBase(
            compd.typed(
              if (name == "normal") compd.CompType.N else compd.CompType.A,
            ),
            purd.EFlat(v),
            purd.EFlat(Str(target)),
          ),
        )
      case (
            Elem(purd.EFlat(v: PureValue), _, _, _),
            Elem(purd.EFlat(CONST_EMPTY), _, _, _),
          ) =>
        Elem(compt =
          compd.EBase(
            compd.typed(
              if (name == "normal") compd.CompType.N else compd.CompType.A,
            ),
            purd.EFlat(v),
            purd.EFlat(CONST_EMPTY),
          ),
        )
      case (Elem(purd.Bot, _, _, _), _) | (_, Elem(purd.Bot, _, _, _)) =>
        Bot
      case (_, _) =>
        Elem(
          compt = compd.Top,
        )
    }

  // Set(ASTType | NumType | BoolType | AddType | EtcType) * (CloType)             * (ContType)
  // (Value | SyntacticView)                               * (Set[AClo] | ATopClo) * (Set[ACont] | ATopClo)
  // Bot

  case class Elem(
    pure: purd.Elem = purd.Bot,
    clo: clod.Elem = clod.Bot,
    cont: contd.Elem = contd.Bot,
    compt: compd.Elem = compd.Bot,
  ) extends AbsValueTrait {

    def removeNormal: Elem = compt match
      case compd.EBot => Bot
      case compd.EBase(ty, v, t) =>
        if (v.isBottom || t.isBottom) Bot
        else
          ty.getSingle match
            case FlatTop =>
              Elem(compt = compd.EBase(compd.typed(compd.CompType.A), v, t))
            case FlatElem(compd.CompType.A) => Elem(compt = compt)
            case _                          => Bot

    def normal: Elem = compt match
      case compd.EBot => Bot
      case compd.EBase(ty, v, _) =>
        ty.getSingle match
          case FlatTop                    => Elem(pure = v)
          case FlatElem(compd.CompType.N) => Elem(pure = v)
          case _                          => Bot

    def isAbruptCompletion: Elem = if (this.isBottom) Bot
    else if (compt.isBottom) Elem(pure = purd.EFlat(Bool(false)))
    else if (
      pure.isBottom && clo.isBottom && cont.isBottom && (compt match {
        case compd.EBase(ty, _, _) => ty.getSingle == FlatElem(compd.CompType.A)
        case _                     => false
      })
    ) Elem(pure = purd.EFlat(Bool(true)))
    else Elem(pure = purd.EKind(Set(purd.ValueKind.Bool)))
    def isCompletion: Elem = if (this.isBottom) Bot
    else if (compt.isBottom) Elem(pure = purd.EFlat(Bool(false)))
    else if (pure.isBottom && clo.isBottom && cont.isBottom)
      Elem(pure = purd.EFlat(Bool(true)))
    else Elem(pure = purd.EKind(Set(purd.ValueKind.Bool)))

    def escaped: Elem = this.copy(
      pure = pure ⊔ (compt match {
        case compd.EBot           => purd.Bot
        case compd.EBase(_, v, _) => v
      }),
      compt = compd.Bot,
    )
    def wrapCompletion: Elem = if (pure.isBottom) {
      if (compt.isBottom) Bot else Elem(compt = compt)
    } else
      this.copy(
        pure = purd.Bot,
        compt = compd.EBase(
          compd.typed(compd.CompType.N),
          pure,
          purd.EFlat(CONST_EMPTY),
        ) ⊔ compt,
      )

    def getHandler = clo match {
      case clod.EHandler(name, _, f) => Some((name, f))
      case _                         => None
    }

    def unary_! : Elem = Elem(
      pure = pure match
        case purd.EKind(_)         => purd.EKind(Set(purd.ValueKind.Bool))
        case (purd.EFlat(Bool(b))) => purd.EFlat(Bool(!b))
        case _                     => purd.Bot,
    )
    def ||(that: Elem): Elem = Elem(
      pure = (this.pure, that.pure) match {
        case (purd.EKind(_), _) | (_, purd.EKind(_)) =>
          purd.EKind(Set(purd.ValueKind.Bool))
        case (purd.EFlat(Bool(true)), _) | (_, purd.EFlat(Bool(true))) =>
          purd.EFlat(Bool(true))
        case (purd.EFlat(Bool(false)), purd.EFlat(Bool(false))) =>
          purd.EFlat(Bool(false))
        case (purd.EBot, _) | (_, purd.EBot) => purd.EBot
        case (_, _)                          => purd.EFlat(Bool(false))
      },
    )
    def &&(that: Elem): Elem = Elem(pure = purd.Top)

    def =^=(that: Elem): Elem = Elem(
      pure = (this.pure, that.pure) match {
        case (purd.EKind(_), _) | (_, purd.EKind(_)) =>
          purd.EKind(Set(purd.ValueKind.Bool))
        case (purd.EFlat(x), purd.EFlat(y))  => purd.EFlat(Bool(x == y))
        case (purd.EBot, _) | (_, purd.EBot) => purd.EBot
      },
    )

    def mul(that: Elem): Elem = Elem(pure = purd.Top)
    def plus(that: Elem): Elem = Elem(pure = purd.Top)
    def min(that: Elem): Elem = Elem(pure = purd.Top)
    def max(that: Elem): Elem = Elem(pure = purd.Top)

    def projValueKind(v: purd.ValueKind) = pure match {
      case f: purd.EFlat =>
        if (f.kind == v)
          this.copy(
            pure = f,
          )
        else Bot
      case s: purd.EKind =>
        if (s.s contains v)
          this.copy(
            pure = purd.EKind(Set(v)),
          )
        else Bot
      case purd.EBot => Bot
    }

    def project[T <: AValue](kinds: AValueKind[T]): Elem =
      kinds match
        case CloKind  => Elem(clo = clo)
        case ContKind => Elem(cont = cont)
        case CompKind => Elem(compt = compt)
        case LocKind =>
          projValueKind(purd.ValueKind.Symbol) ⊔ projValueKind(
            purd.ValueKind.Obj,
          ) ⊔ projValueKind(purd.ValueKind.List) ⊔ projValueKind(
            purd.ValueKind.Record,
          )
        case SpecLocKind =>
          projValueKind(purd.ValueKind.List) ⊔ projValueKind(
            purd.ValueKind.Record,
          )
        case ListLocKind   => projValueKind(purd.ValueKind.List)
        case RecordLocKind => projValueKind(purd.ValueKind.Record)
        case ObjLocKind    => projValueKind(purd.ValueKind.Obj)
        case SymbolLocKind => projValueKind(purd.ValueKind.Symbol)
        case StrKind       => projValueKind(purd.ValueKind.Str)
        case NumKind       => projValueKind(purd.ValueKind.Num)
        case AllKind       => this
        case BoolKind      => projValueKind(purd.ValueKind.Bool)
        case BigIntKind    => projValueKind(purd.ValueKind.BigInt)
        case NullKind      => projValueKind(purd.ValueKind.Null)
        case UndefKind     => projValueKind(purd.ValueKind.Undef)
        case LiteralKind =>
          projValueKind(purd.ValueKind.Str) ⊔ projValueKind(
            purd.ValueKind.Num,
          ) ⊔ projValueKind(purd.ValueKind.Bool) ⊔ projValueKind(
            purd.ValueKind.Null,
          ) ⊔ projValueKind(purd.ValueKind.Undef) ⊔ projValueKind(
            purd.ValueKind.BigInt,
          ) ⊔ projValueKind(
            purd.ValueKind.Etc,
          )
        case _ => this

    def getSingle[T <: AValue](kind: AValueKind[T]): Flat[T] =
      kind match
        case CloKind =>
          clo match
            case clod.ETopClo =>
              throw new Error(s"Exploded $this $kind")
            case clod.EIgnoreClo =>
              FlatTop
            case clod.ESet(v: Set[(AClo, Map[Int, Elem])]) =>
              if (v.size == 1) FlatElem(v.head._1)
              else if (v.size == 0) FlatBot
              else FlatTop
            case clod.EHandler(_, _, _) => FlatTop
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
            case purd.EFlat(asite: AllocSite) =>
              (kind extractLift (Loc(asite)))
                .map(FlatElem(_))
                .getOrElse(FlatBot)

    def getBoundedCloSet: Set[(AClo, Map[Int, Elem])] =
      clo match
        case clod.ETopClo =>
          throw new Error(s"Exploded $this CloKind")
        case clod.EIgnoreClo =>
          Set()
        case clod.ESet(v: Set[(AClo, Map[Int, Elem])]) => v
        case clod.EHandler(_, _, _)                    => Set()

    def getSet[T <: AValue](kind: AValueKind[T]): Set[T] =
      kind match
        case CloKind =>
          clo match
            case clod.ETopClo =>
              throw new Error(s"Exploded $this $kind")
            case clod.EIgnoreClo =>
              Set()
            case clod.ESet(v: Set[(AClo, Map[Int, Elem])]) => v.map(_._1)
            case clod.EHandler(_, _, _)                    => Set()
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
      this.pure ⊑ that.pure && this.clo ⊑ that.clo && this.cont ⊑ that.cont && this.compt ⊑ that.compt

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

    override def beautify(grammar: Option[esmeta.spec.Grammar]): String =
      s"(${pure.beautify(grammar)}, ${clo
        .beautify(grammar)}, ${cont.beautify(grammar)}, ${compt.beautify(grammar)})"
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
