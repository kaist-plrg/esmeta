package esmeta.editor.analyzer

import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

class PairDomain[T <: Domain, S <: Domain](val ld: T, val bd: S) extends Domain:

  val Bot = EBot
  val TopOpt = ld.TopOpt.flatMap((l) => bd.TopOpt.map((b) => EBase(l, b)))

  sealed trait Elem extends ElemTrait {
    def ⊑(that: Elem): Boolean = (this, that) match
      case (EBot, _)              => true
      case (_, EBot)              => false
      case (e1: EBase, e2: EBase) => e1.left ⊑ e2.left && e1.right ⊑ e2.right
    def ⊔(that: Elem): Elem = (this, that) match
      case (EBot, _) => that
      case (_, EBot) => this
      case (e1: EBase, e2: EBase) =>
        EBase(e1.left ⊔ e2.left, e1.right ⊔ e2.right)
  }

  case class EBase(left: ld.Elem, right: bd.Elem) extends Elem {
    assert(!left.isBottom && !right.isBottom)
  }

  case object EBot extends Elem

  implicit val app: Rule[Elem] = (app, elem) =>
    app >> {
      elem match {
        case EBase(l, r) => s"($l, $r)"
        case EBot        => "⊥"
      }
    } >> ""

end PairDomain
