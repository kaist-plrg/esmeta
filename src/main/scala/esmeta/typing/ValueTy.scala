package esmeta.typing

import esmeta.state.*
import esmeta.util.*
import java.lang.Character.Subset

/** value types */
case class ValueTy(
  comp: CompTy,
  pureValue: PureValueTy,
  subMap: SubMapTy,
) extends Ty {

  /** bottom check */
  def isBottom: Boolean =
    this.comp.isBottom &
    this.pureValue.isBottom &
    this.subMap.isBottom

  /** union type */
  def |(that: ValueTy): ValueTy = ValueTy(
    this.comp | that.comp,
    this.pureValue | that.pureValue,
    this.subMap | that.subMap,
  )

  /** intersection type */
  def &(that: ValueTy): ValueTy = ValueTy(
    this.comp & that.comp,
    this.pureValue & that.pureValue,
    this.subMap & that.subMap,
  )

  /** prune type */
  def --(that: ValueTy): ValueTy = ValueTy(
    this.comp -- that.comp,
    this.pureValue -- that.pureValue,
    this.subMap -- that.subMap,
  )

  /** completion check */
  def isCompletion: Boolean =
    !comp.isBottom &
    pureValue.isBottom &
    subMap.isBottom
}
object ValueTy {
  def apply(
    comp: CompTy = CompTy(),
    normal: PureValueTy = PureValueTy(),
    abrupt: Boolean = false,
    pureValue: PureValueTy = PureValueTy(),
    clo: CloTy = CloTy(),
    cont: ContTy = ContTy(),
    record: RecordTy = RecordTy(),
    list: ListTy = ListTy(),
    symbol: Boolean = false,
    astValue: BSet[String] = Fin(),
    grammar: BSet[Grammar] = Fin(),
    codeUnit: Boolean = false,
    const: Set[String] = Set(),
    math: Boolean = false,
    number: Boolean = false,
    bigInt: Boolean = false,
    str: BSet[String] = Fin(),
    bool: Set[Boolean] = Set(),
    undef: Boolean = false,
    nullv: Boolean = false,
    absent: Boolean = false,
    subMap: SubMapTy = SubMapTy(),
  ): ValueTy = ValueTy(
    comp = comp | CompTy(normal, abrupt),
    pureValue = pureValue | PureValueTy(
      clo,
      cont,
      record,
      list,
      symbol,
      astValue,
      grammar,
      codeUnit,
      const,
      math,
      number,
      bigInt,
      str,
      bool,
      undef,
      nullv,
      absent,
    ),
    subMap = subMap,
  )
}