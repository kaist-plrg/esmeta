package esmeta.editor.analyzer

import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

class AContDomain[T <: AbsValueDomain with Singleton](val avd: T)
  extends Domain {

  val Bot = ESet(Set())
  val TopOpt = Some(ETopCont)

  sealed trait Elem extends ElemTrait {
    def ⊑(that: Elem): Boolean = (this, that) match
      case (_, ETopCont)              => true
      case (ETopCont, _)              => false
      case (_: ESet, EIgnoreCont)     => true
      case (EIgnoreCont, _: ESet)     => false
      case (EIgnoreCont, EIgnoreCont) => true
      case (ESet(s1), ESet(s2)) =>
        (s1.map(_.func.name) ++ s2.map(_.func.name)).forall {
          case key =>
            (s1.find(_.func.name == key), s2.find(_.func.name == key)) match {
              case (None, _) => true
              case (Some(p1), Some(p2)) =>
                (p1.captured.keySet ++ p2.captured.keySet).forall {
                  case name =>
                    (p1.captured.get(name), p2.captured.get(name)) match {
                      case (None, _)            => true
                      case (Some(a1), Some(a2)) => a1 ⊑ a2
                      case (_, None)            => false
                    }
                }
              case (_, None) => false
            }
        }

    def ⊔(that: Elem): Elem = (this, that) match
      case (_, ETopCont)              => that
      case (ETopCont, _)              => this
      case (_: ESet, EIgnoreCont)     => that
      case (EIgnoreCont, _: ESet)     => this
      case (EIgnoreCont, EIgnoreCont) => this
      case (ESet(s1), ESet(s2)) =>
        ESet((s1.map(_.func.name) ++ s2.map(_.func.name)).map {
          case key =>
            (s1.find(_.func.name == key), s2.find(_.func.name == key)) match {
              case (None, Some(p2)) => p2
              case (Some(p1), Some(p2)) =>
                avd.ACont(
                  p1.func,
                  (p1.captured.keySet ++ p2.captured.keySet).map {
                    case name =>
                      name -> ((
                        p1.captured.get(name),
                        p2.captured.get(name),
                      ) match {
                        case (None, Some(x))      => x
                        case (Some(a1), Some(a2)) => a1 ⊔ a2
                        case (Some(x), None)      => x
                        case (None, None)         => throw Error("unreachable")
                      })
                  }.toMap,
                )
              case (Some(p1), None) => p1
              case (None, None)     => throw Error("unreachable")
            }
        })

    override def beautify(grammar: Option[esmeta.spec.Grammar]): String =
      this match
        case ESet(s) =>
          if (s.size == 0) "⊥" else s.map(_.toString).mkString("{", ", ", "}")
        case EIgnoreCont => "!⊤!"
        case ETopCont    => "⊤"

  }
  case class ESet(s: Set[avd.ACont]) extends Elem
  case object EIgnoreCont extends Elem
  case object ETopCont extends Elem

  implicit val app: Rule[Elem] = (app, elem) => app >> elem.toString
}
