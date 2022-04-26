package esmeta.editor.analyzer

import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

class ACloDomain[T <: AbsValueDomain with Singleton](val avd: T)
  extends Domain {

  val Bot = ESet(Set())

  sealed trait Elem extends ElemTrait {
    def ⊑(that: Elem): Boolean = (this, that) match
      case (_, ETopClo)                => true
      case (ETopClo, _)                => false
      case (_: ESet, EIgnoreClo)       => true
      case (_: EHandler, EIgnoreClo)   => true
      case (_: EHandler, _: ESet)      => false
      case (s: ESet, _: EHandler)      => s.s.size == 0
      case (h: EHandler, h2: EHandler) => h.name == h2.name
      case (EIgnoreClo, _: EHandler)   => false
      case (EIgnoreClo, _: ESet)       => false
      case (EIgnoreClo, EIgnoreClo)    => true
      case (ESet(s1), ESet(s2)) =>
        (s1.map(_._1.func.name) ++ s2.map(_._1.func.name)).forall {
          case key =>
            (
              s1.find(_._1.func.name == key),
              s2.find(_._1.func.name == key),
            ) match {
              case (None, _) => true
              case (Some((p1, _)), Some((p2, _))) =>
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
      case (_, ETopClo)                                      => that
      case (ETopClo, _)                                      => this
      case (_: ESet, EIgnoreClo) | (_: EHandler, EIgnoreClo) => that
      case (EIgnoreClo, _: ESet) | (EIgnoreClo, _: EHandler) => this
      case (h: EHandler, s: ESet)   => if (s.s.size == 0) h else EIgnoreClo
      case (s: ESet, h: EHandler)   => if (s.s.size == 0) h else EIgnoreClo
      case (EIgnoreClo, EIgnoreClo) => this
      case (h1: EHandler, h2: EHandler) =>
        if (h1.name == h2.name) this else EIgnoreClo
      case (ESet(s1), ESet(s2)) =>
        ESet((s1.map(_._1.func.name) ++ s2.map(_._1.func.name)).map {
          case key =>
            (
              s1.find(_._1.func.name == key),
              s2.find(_._1.func.name == key),
            ) match {
              case (None, Some(p2)) => p2
              case (Some(p1), Some(p2)) =>
                (
                  avd.AClo(
                    p1._1.func,
                    (p1._1.captured.keySet ++ p2._1.captured.keySet).map {
                      case name =>
                        name -> ((
                          p1._1.captured.get(name),
                          p2._1.captured.get(name),
                        ) match {
                          case (None, Some(x))      => x
                          case (Some(a1), Some(a2)) => a1 ⊔ a2
                          case (Some(x), None)      => x
                          case (None, None) => throw Error("unreachable")
                        })
                    }.toMap,
                  ),
                  p1._2,
                )
              case (Some(p1), None) => p1
              case (None, None)     => throw Error("unreachable")
            }
        })

    override def beautify(grammar: Option[esmeta.spec.Grammar]) = this match
      case ESet(s) =>
        if (s.size == 0) "⊥" else s.map(_.toString).mkString("{", ", ", "}")
      case EHandler(name, kind, _) => s"${name}[$kind]"
      case EIgnoreClo              => "!⊤!"
      case ETopClo                 => "⊤"

  }
  case class EHandler(name: String, kind: String, f: List[avd.Elem] => avd.Elem)
    extends Elem
  case class ESet(s: Set[(avd.AClo, Map[Int, avd.Elem])]) extends Elem
  case object EIgnoreClo extends Elem
  case object ETopClo extends Elem

  implicit val app: Rule[Elem] = (app, elem) => app >> elem.beautify(None)
}
