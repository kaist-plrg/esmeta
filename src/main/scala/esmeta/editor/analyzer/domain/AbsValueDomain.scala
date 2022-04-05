package esmeta.editor.analyzer

import esmeta.ir.Id
import esmeta.interp.*
import esmeta.error.ESMetaError
import esmeta.cfg.Func
import esmeta.ir.Name
import esmeta.js.Ast
import esmeta.cfg.Node
import esmeta.editor.sview.SyntacticView

trait AbsValueDomain extends Domain {

  val Top: Elem
  type Elem <: AbsValueTrait
  def apply(value: Value): Elem = this(AValue.from(value))
  def apply(value: AValue): Elem

  def fromAValues[T <: AValue](kind: AValueKind[T])(items: T*): Elem
  def mkAbsComp(name: String, value: Elem, target: Elem): Elem

  sealed trait AbsRefValue
  case class AbsIdValue(id: Id) extends AbsRefValue
  case class AbsPropValue(r: Elem, p: Elem) extends AbsRefValue

  // values used in analysis
  sealed trait AValue {
    // conversion to string
    override def toString: String = this match {
      case AComp(ALiteral(Const("noraml")), value, _) => s"N($value)"
      case AComp(ty, value, target) => s"C($ty, $value, $target)"
      // case AConst(name)                      => s"~$name~"
      case NamedLoc(name)     => s"#$name"
      case AllocSite(k, view) => s"#$k:$view"
      case SubMapLoc(baseLoc) => s"$baseLoc:SubMap"
      case AClo(func, captured) => (
        func.irFunc.params.mkString("(", ", ", ")") +
        (for ((x, v) <- captured) yield s"$x -> $v").mkString("[", ", ", "]") +
        s" => ${func.name}"
      )
      case ACont(func, captured, target) =>
        s"${func.irFunc.params.mkString("(", ", ", ")")} [=>] $target"
      case AAst(ast) =>
        val max = AValue.AST_MAX_LENGTH
        var str = ast.toString
        if (str.length > max) str = str.substring(0, max - 3) + "..."
        f"☊[${ast.name}]($str) @ 0x${ast.hashCode}%08x"
      case ASView(view) =>
        val max = AValue.AST_MAX_LENGTH
        var str = view.toString
        if (str.length > max) str = str.substring(0, max - 3) + "..."
        f"☊[${view.name}]($str)"
      case ALiteral(literal) => literal.toString
    }
  }
  object AValue {
    val AST_MAX_LENGTH = 10

    // from original concrete values
    def from(value: Value): AValue = value match {
      case Comp(Const(name), value, targetOpt) =>
        AComp(
          ALiteral(Const(name)),
          from(value),
          targetOpt.fold[AValue](ALiteral(Const("empty")))(str =>
            ALiteral(Str(str)),
          ),
        )
      case addr: Addr => Loc.from(addr)
      case Clo(func, captured) =>
        AClo(func, captured.map((k, v) => (k, apply(from(v)))))
      case AstValue(ast)         => AAst(ast)
      case literal: LiteralValue => ALiteral(literal)
      case _ =>
        throw new ESMetaError(s"impossible to convert to AValue: $value")
    }
  }

  // completions
  case class AComp(ty: ALiteral[Const], value: AValue, target: AValue)
    extends AValue

  // abstract locations for addresses
  sealed trait Loc extends AValue {
    // check named locations
    def isNamed: Boolean = this match {
      case NamedLoc(_) | SubMapLoc(NamedLoc(_)) => true
      case _                                    => false
    }

    // get base locations
    def base: BaseLoc = this match {
      case base: BaseLoc   => base
      case SubMapLoc(base) => base
    }
  }
  object Loc {
    // from original concrete addresses
    private val subMapPattern = "(.+).SubMap".r
    def from(addr: Addr): Loc = addr match {
      case NamedAddr(name) =>
        name match {
          case subMapPattern(base) => SubMapLoc(NamedLoc(base))
          case name                => NamedLoc(name)
        }
      case _ => throw new ESMetaError(s"impossible to convert to Loc: $addr")
    }
  }
  sealed trait BaseLoc extends Loc
  case class NamedLoc(name: String) extends BaseLoc
  case class AllocSite(k: Int, view: View) extends BaseLoc
  case class SubMapLoc(baseLoc: BaseLoc) extends Loc

  // closures
  case class AClo(
    func: Func,
    captured: Map[Name, Elem],
  ) extends AValue

  // continuations
  case class ACont(
    func: Func,
    captured: Map[Name, Elem],
    target: NodePoint[Node],
  ) extends AValue

  sealed trait ASyntactic extends AValue
  // AST values
  case class AAst(ast: Ast) extends ASyntactic

  // Syntacticview values
  case class ASView(view: SyntacticView) extends ASyntactic

  // literal values
  case class ALiteral[+T <: LiteralValue](simple: T) extends AValue

  sealed trait AValueKind[+T <: AValue] {
    def extract: PartialFunction[AValue, T]
    def extractLift = extract.lift
  }
  case object AllKind extends AValueKind[AValue] {
    def extract = { case x: AValue => x }
  }
  case object CompKind extends AValueKind[AComp] {
    def extract = { case x: AComp => x }
  }
  case object ConstKind extends AValueKind[ALiteral[Const]] {
    def extract = { case ALiteral(Const(name)) => ALiteral(Const(name)) }
  }
  case object LocKind extends AValueKind[Loc] {
    def extract = { case x: Loc => x }
  }
  case object CloKind extends AValueKind[AClo] {
    def extract = { case x: AClo => x }
  }
  case object ContKind extends AValueKind[ACont] {
    def extract = { case x: ACont => x }
  }
  case object AstKind extends AValueKind[ASyntactic] {
    def extract = { case x: ASyntactic => x }
  }
  case object LiteralKind extends AValueKind[ALiteral[LiteralValue]] {
    def extract = { case x @ ALiteral(_) => x }
  }
  case object MathKind extends AValueKind[ALiteral[Math]] {
    def extract = { case ALiteral(Math(x)) => ALiteral(Math(x)) }
  }
  case object NumKind extends AValueKind[ALiteral[Number]] {
    def extract = { case ALiteral(Number(x)) => ALiteral(Number(x)) }
  }
  case object BigIntKind extends AValueKind[ALiteral[BigInt]] {
    def extract = { case ALiteral(BigInt(x)) => ALiteral(BigInt(x)) }
  }
  case object StrKind extends AValueKind[ALiteral[Str]] {
    def extract = { case ALiteral(Str(x)) => ALiteral(Str(x)) }
  }
  case object BoolKind extends AValueKind[ALiteral[Bool]] {
    def extract = { case ALiteral(Bool(x)) => ALiteral(Bool(x)) }
  }
  case object UndefKind extends AValueKind[ALiteral[Undef.type]] {
    def extract = { case ALiteral(Undef) => ALiteral(Undef) }
  }
  case object NullKind extends AValueKind[ALiteral[Null.type]] {
    def extract = { case ALiteral(Null) => ALiteral(Null) }
  }
  case object AbsentKind extends AValueKind[ALiteral[Absent.type]] {
    def extract = { case ALiteral(Absent) => ALiteral(Absent) }
  }

  trait AbsValueTrait extends ElemTrait { this: Elem =>

    def removeNormal: Elem
    def normal: Elem
    def isAbruptCompletion: Elem

    def unary_! : Elem
    def ||(that: Elem): Elem
    def &&(that: Elem): Elem

    def =^=(that: Elem): Elem

    def mul(that: Elem): Elem
    def plus(that: Elem): Elem

    def isCompletion: Elem
    def project(kinds: AValueKind[AValue]*): Elem
    def getSingle[T <: AValue](kind: AValueKind[T] = AllKind): Flat[T]
    def getSet[T <: AValue](kind: AValueKind[T] = AllKind): Set[T]
    def escaped: Elem
    def wrapCompletion: Elem
  }
}
