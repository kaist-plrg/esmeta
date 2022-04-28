package esmeta.editor.analyzer

import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

class CompDomain[T <: PureValueDomain](val purd: T) extends Domain:

  val ecompd = PairDomain[purd.type, purd.type](purd, purd)

  val Bot = EBase(ecompd.Bot, ecompd.Bot)
  val TopOpt = ecompd.TopOpt.map((t) => EBase(t, t))
  val Top = TopOpt.get

  sealed trait Elem extends ElemTrait {
    def ⊑(that: Elem): Boolean = (this, that) match
      case (e1: EBase, e2: EBase) => e1.ncomp ⊑ e2.ncomp && e1.acomp ⊑ e2.acomp
    def ⊔(that: Elem): Elem = (this, that) match
      case (e1: EBase, e2: EBase) =>
        EBase(e1.ncomp ⊔ e2.ncomp, e1.acomp ⊔ e2.acomp)
  }

  case class EBase(ncomp: ecompd.Elem, acomp: ecompd.Elem) extends Elem

  implicit val app: Rule[Elem] = (app, elem) =>
    app >> {
      elem match {
        case EBase(ncomp, acomp) => s"(N: $ncomp, A: $acomp)"
      }
    } >> ""

end CompDomain
