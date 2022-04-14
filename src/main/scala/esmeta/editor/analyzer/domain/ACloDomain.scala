package esmeta.editor.analyzer

import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

class ACloDomain[T <: AbsValueDomain with Singleton](val avd: T)
  extends Domain {

  val Bot = ESet(Set())

  sealed trait Elem extends ElemTrait {
    def ⊑(that: Elem): Boolean = (this, that) match
      case (_, ETopClo)             => true
      case (ETopClo, _)             => false
      case (_: ESet, EIgnoreClo)    => true
      case (EIgnoreClo, _: ESet)    => false
      case (EIgnoreClo, EIgnoreClo) => true
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
      case (_, ETopClo)             => that
      case (ETopClo, _)             => this
      case (_: ESet, EIgnoreClo)    => that
      case (EIgnoreClo, _: ESet)    => this
      case (EIgnoreClo, EIgnoreClo) => this
      case (ESet(s1), ESet(s2)) =>
        ESet((s1.map(_.func.name) ++ s2.map(_.func.name)).map {
          case key =>
            (s1.find(_.func.name == key), s2.find(_.func.name == key)) match {
              case (None, Some(p2)) => p2
              case (Some(p1), Some(p2)) =>
                avd.AClo(
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

  }
  case class ESet(s: Set[avd.AClo]) extends Elem
  case object EIgnoreClo extends Elem
  case object ETopClo extends Elem

  implicit val app: Rule[Elem] = (app, elem) => app >> elem.toString
}
