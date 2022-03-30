package esmeta.editor.analyzer

import esmeta.ir.Id
import esmeta.interp.*
import esmeta.error.ESMetaError
import esmeta.cfg.Func
import esmeta.ir.Name
import esmeta.js.Ast
import esmeta.cfg.Node

trait AbsValueDomain extends Domain {

  type Elem <: AbsValueTrait
  def apply(value: Value): Elem = this(AValue.from(value))
  def apply(value: AValue): Elem

  def fromAValues[T <: AValue](kind: AValueKind[T])(items: T*): Elem
  def mkAbsComp(name: String, value: Elem, target: Elem): Elem

  sealed trait AbsRefValue
  case class AbsRefGlobal(id: String) extends AbsRefValue
  case class AbsRefName(id: String) extends AbsRefValue
  case class AbsRefTemp(id: Int) extends AbsRefValue
  case class AbsRefProp(r: Elem, p: Elem) extends AbsRefValue

  // values used in analysis
  sealed trait AValue {
    // conversion to string
    override def toString: String = this match {
      case AComp(AConst("noraml"), value, _) => s"N($value)"
      case AComp(ty, value, target)          => s"C($ty, $value, $target)"
      case AConst(name)                      => s"~$name~"
      case NamedLoc(name)                    => s"#$name"
      case AllocSite(k, view)                => s"#$k:$view"
      case SubMapLoc(baseLoc)                => s"$baseLoc:SubMap"
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
      case ALiteral(literal) => literal.toString
    }
  }
  object AValue {
    val AST_MAX_LENGTH = 10

    // from original concrete values
    def from(value: Value): AValue = value match {
      case Comp(Const(name), value, targetOpt) =>
        AComp(
          AConst(name),
          from(value),
          targetOpt.fold[AValue](AConst("empty"))(str => ALiteral(Str(str))),
        )
      case Const(name) => AConst(name)
      case addr: Addr  => Loc.from(addr)
      case Clo(func, captured) =>
        AClo(func, captured.map((k, v) => (k, apply(from(v)))))
      case AstValue(ast)         => AAst(ast)
      case literal: LiteralValue => ALiteral(literal)
      case _ =>
        throw new ESMetaError(s"impossible to convert to AValue: $value")
    }
  }

  // completions
  case class AComp(ty: AConst, value: AValue, target: AValue) extends AValue

  // constants
  case class AConst(name: String) extends AValue

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

  // AST values
  case class AAst(ast: Ast) extends AValue

  // literal values
  case class ALiteral[+T <: LiteralValue](simple: T) extends AValue

  sealed trait AValueKind[+T <: AValue]
  case object AllKind extends AValueKind[AValue]
  case object CompKind extends AValueKind[AComp]
  case object ConstKind extends AValueKind[AConst]
  case object LocKind extends AValueKind[Loc]
  case object CloKind extends AValueKind[AClo]
  case object ContKind extends AValueKind[ACont]
  case object AstKind extends AValueKind[AAst]
  case object LiteralKind extends AValueKind[ALiteral[LiteralValue]]
  case object MathKind extends AValueKind[ALiteral[Math]]
  case object NumKind extends AValueKind[ALiteral[Number]]
  case object BigIntKind extends AValueKind[ALiteral[BigInt]]
  case object StrKind extends AValueKind[ALiteral[Str]]
  case object BoolKind extends AValueKind[ALiteral[Bool]]
  case object UndefKind extends AValueKind[ALiteral[Undef.type]]
  case object NullKind extends AValueKind[ALiteral[Null.type]]
  case object AbsentKind extends AValueKind[ALiteral[Absent.type]]

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
