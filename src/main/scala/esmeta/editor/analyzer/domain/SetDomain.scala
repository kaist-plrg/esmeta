package esmeta.editor.analyzer

import esmeta.util.Appender.*
import esmeta.util.Appender

// set domain
class SetDomain[A <: reflect.Enum](val elems: Iterable[A]) extends Domain {

  // elements
  val Bot = Base(Set())

  val Top = Base(elems.toSet)

  // abstraction functions
  def apply(elems: A*): Elem = this(elems)
  def apply(elems: Iterable[A]): Elem = alpha(elems)
  def alpha(elems: Iterable[A]): Elem = Base(elems.toSet)

  // appender
  implicit val app: Rule[Elem] = (app, elem) =>
    elem match {
      case Base(set) =>
        app >> (set.size match {
          case 0 => "⊥"
          case 1 => set.head.toString
          case _ => set.toList.map(_.toString).sorted.mkString("{", ", ", "}")
        })
    }

  // elements
  sealed trait Elem extends Iterable[A] with ElemTrait {
    // partial order
    def ⊑(that: Elem): Boolean = (this, that) match {
      case (Base(lset), Base(rset)) => lset subsetOf rset
    }

    // join operator
    def ⊔(that: Elem): Elem = (this, that) match {
      case (Base(lset), Base(rset)) => Base(lset ++ rset)
    }

    // meet operator
    def ⊓(that: Elem): Elem = (this, that) match {
      case (Base(lset), Base(rset)) => Base(lset intersect rset)
    }

    // get single value
    def getSingle: Flat[A] = this match {
      case Base(set) =>
        set.size match {
          case 0 => FlatBot
          case 1 => FlatElem(set.head)
          case _ => FlatTop
        }
      case _ => FlatTop
    }

    // iterators
    final def iterator: Iterator[A] = (this match {
      case Base(set) => set
    }).iterator

    // contains check
    def contains(elem: A): Boolean = this match {
      case Base(set) => set contains elem
    }
  }
  case class Base(set: Set[A]) extends Elem
}
object SetDomain {
  // constructors
  def apply[A <: reflect.Enum](
    totalOpt: Iterable[A],
  ): SetDomain[A] = {
    new SetDomain[A](totalOpt)
  }
}
