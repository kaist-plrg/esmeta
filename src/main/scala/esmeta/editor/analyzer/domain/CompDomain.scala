package esmeta.editor.analyzer

import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

class CompDomain[T <: PureValueDomain](val purd: T) extends Domain:

  enum CompType:
    case N, A
  end CompType
  val typed = FlatDomain(
    "⊤",
    totalOpt = Some(Set(CompType.N, CompType.A)),
    isExploded = false,
  )

  val Bot = EBot
  val Top = EBase(typed.Top, purd.Top, purd.Top)

  sealed trait Elem extends ElemTrait {
    def ⊑(that: Elem): Boolean = (this, that) match
      case (EBot, _)              => true
      case (_, EBot)              => false
      case (e1: EBase, e2: EBase) => e1.compt ⊑ e2.compt && e1.pure ⊑ e2.pure
    def ⊔(that: Elem): Elem = (this, that) match
      case (EBot, _) => that
      case (_, EBot) => this
      case (e1: EBase, e2: EBase) =>
        EBase(e1.compt ⊔ e2.compt, e1.pure ⊔ e2.pure, e1.target ⊔ e2.target)
  }

  case class EBase(compt: typed.Elem, pure: purd.Elem, target: purd.Elem)
    extends Elem {
    assert(!compt.isBottom && !pure.isBottom && !target.isBottom)
  }

  case object EBot extends Elem

  implicit val app: Rule[Elem] = (app, elem) =>
    app >> {
      elem match {
        case EBase(compt, pure, target) => s"($compt, $pure, $target)"
        case EBot                       => "⊥"
      }
    } >> ""

end CompDomain
