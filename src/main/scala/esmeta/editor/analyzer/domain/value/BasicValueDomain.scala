package esmeta.editor.analyzer

import esmeta.util.Appender.*
import esmeta.util.Appender
import esmeta.interp.*

class BasicValueDomain() extends AbsValueDomain {
  val Bot = Elem(
  )

  def apply(value: AValue): Elem = Elem()

  def fromAValues[T <: AValue](kind: AValueKind[T])(items: T*): Elem = Elem()
  def mkAbsComp(name: String, value: Elem, target: Elem): Elem = Elem()

  case class Elem() extends AbsValueTrait {

    def removeNormal: Elem = this
    def normal: Elem = this
    def isAbruptCompletion: Elem = this

    def unary_! : Elem = this
    def ||(that: Elem): Elem = this
    def &&(that: Elem): Elem = this

    def =^=(that: Elem): Elem = this

    def mul(that: Elem): Elem = this
    def plus(that: Elem): Elem = this

    def isCompletion: Elem = this
    def project(kinds: AValueKind[AValue]*): Elem = this
    def getSingle[T <: AValue](kind: AValueKind[T]): Flat[T] = FlatTop
    def getSet[T <: AValue](kind: AValueKind[T]): Set[T] = kind match {
      case _ => Set()
    }
    def escaped: Elem = this
    def wrapCompletion: Elem = this

    def ⊑(that: Elem): Boolean = (true)

    // join operator
    def ⊔(that: Elem): Elem = Elem(
    )

    // conversion to string
    def toString(detail: Boolean): String = {
      val app = new Appender
      app >> this
      app.toString
    }
  }

  // appender
  implicit val app: Rule[Elem] = (app, elem) => app >> ""

}
