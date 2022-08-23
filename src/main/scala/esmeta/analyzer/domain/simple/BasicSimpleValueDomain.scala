package esmeta.analyzer.domain

import esmeta.state.*
import esmeta.util.Appender.*

/** basic abstract domain for simple value */
object BasicSimpleValueDomain extends Domain {
  val Bot = Elem(
    num = AbsNum.Bot,
    bigInt = AbsBigInt.Bot,
    str = AbsStr.Bot,
    bool = AbsBool.Bot,
    undef = AbsUndef.Bot,
    nullv = AbsNull.Bot,
    absent = AbsAbsent.Bot,
  )
  lazy val Top = Elem(
    num = AbsNum.Top,
    bigInt = AbsBigInt.Top,
    str = AbsStr.Top,
    bool = AbsBool.Top,
    undef = AbsUndef.Top,
    nullv = AbsNull.Top,
    absent = AbsAbsent.Top,
  )

  // abstraction functions
  def apply(num: Number): Elem = Bot.copy(num = AbsNum(num))
  def apply(num: Double): Elem = Bot.copy(num = AbsNum(Number(num)))
  def apply(bigInt: BigInt): Elem = Bot.copy(bigInt = AbsBigInt(bigInt))
  def apply(bigInt: scala.math.BigInt): Elem =
    Bot.copy(bigInt = AbsBigInt(BigInt(bigInt)))
  def apply(str: String): Elem = Bot.copy(str = AbsStr(Str(str)))
  def apply(bool: Boolean): Elem = Bot.copy(bool = AbsBool(Bool(bool)))
  lazy val num: Elem = Bot.copy(num = AbsNum.Top)
  lazy val bigInt: Elem = Bot.copy(bigInt = AbsBigInt.Top)
  lazy val str: Elem = Bot.copy(str = AbsStr.Top)
  lazy val bool: Elem = Bot.copy(bool = AbsBool.Top)
  lazy val undef: Elem = Bot.copy(undef = AbsUndef.Top)
  lazy val nullv: Elem = Bot.copy(nullv = AbsNull.Top)
  lazy val absent: Elem = Bot.copy(absent = AbsAbsent.Top)
  def apply(simple: ASimple): Elem = simple.value match
    case num: Number    => this(num)
    case BigInt(bigInt) => this(bigInt)
    case Str(str)       => this(str)
    case Bool(bool)     => this(bool)
    case Undef          => this.undef
    case Null           => this.nullv
    case Absent         => this.absent

  // constructors
  def apply(
    num: AbsNum = AbsNum.Bot,
    bigInt: AbsBigInt = AbsBigInt.Bot,
    str: AbsStr = AbsStr.Bot,
    bool: AbsBool = AbsBool.Bot,
    undef: AbsUndef = AbsUndef.Bot,
    nullv: AbsNull = AbsNull.Bot,
    absent: AbsAbsent = AbsAbsent.Bot,
  ): Elem = Elem(num, bigInt, str, bool, undef, nullv, absent)

  // extractors
  def unapply(elem: Elem) = Some(
    (
      elem.num,
      elem.bigInt,
      elem.str,
      elem.bool,
      elem.undef,
      elem.nullv,
      elem.absent,
    ),
  )

  // appender
  given rule: Rule[Elem] = (app, elem) => {
    if (elem.isBottom) app >> "⊥"
    else {
      val Elem(num, bigInt, str, bool, undef, nullv, absent) = elem
      var strs = Vector[String]()
      if (!num.isBottom) strs :+= num.toString
      if (!bigInt.isBottom) strs :+= bigInt.toString
      if (!str.isBottom) strs :+= str.toString
      if (!bool.isBottom) strs :+= bool.toString
      if (!undef.isBottom) strs :+= undef.toString
      if (!nullv.isBottom) strs :+= nullv.toString
      if (!absent.isBottom) strs :+= absent.toString
      app >> strs.mkString(", ")
    }
  }

  // elements
  case class Elem(
    num: AbsNum,
    bigInt: AbsBigInt,
    str: AbsStr,
    bool: AbsBool,
    undef: AbsUndef,
    nullv: AbsNull,
    absent: AbsAbsent,
  ) extends ElemTrait {
    // partial order
    def ⊑(that: Elem): Boolean = (
      this.num ⊑ that.num &&
        this.bigInt ⊑ that.bigInt &&
        this.str ⊑ that.str &&
        this.bool ⊑ that.bool &&
        this.undef ⊑ that.undef &&
        this.nullv ⊑ that.nullv &&
        this.absent ⊑ that.absent
    )

    // join operator
    def ⊔(that: Elem): Elem = Elem(
      this.num ⊔ that.num,
      this.bigInt ⊔ that.bigInt,
      this.str ⊔ that.str,
      this.bool ⊔ that.bool,
      this.undef ⊔ that.undef,
      this.nullv ⊔ that.nullv,
      this.absent ⊔ that.absent,
    )

    // meet operator
    def ⊓(that: Elem): Elem = Elem(
      this.num ⊓ that.num,
      this.bigInt ⊓ that.bigInt,
      this.str ⊓ that.str,
      this.bool ⊓ that.bool,
      this.undef ⊓ that.undef,
      this.nullv ⊓ that.nullv,
      this.absent ⊓ that.absent,
    )

    // minus operator
    def -(that: Elem): Elem = Elem(
      this.num - that.num,
      this.bigInt - that.bigInt,
      this.str - that.str,
      this.bool - that.bool,
      this.undef - that.undef,
      this.nullv - that.nullv,
      this.absent - that.absent,
    )

    // get single value
    def getSingle: Flat[ASimple] = (
      this.num.getSingle ⊔
        this.bigInt.getSingle ⊔
        this.str.getSingle ⊔
        this.bool.getSingle ⊔
        this.undef.getSingle ⊔
        this.nullv.getSingle ⊔
        this.absent.getSingle
    ).map(ASimple.apply)

    // remove absent values
    def removeAbsent: Elem = copy(absent = AbsAbsent.Bot)
  }
}
