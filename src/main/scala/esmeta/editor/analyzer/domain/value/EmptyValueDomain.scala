package esmeta.editor.analyzer

import esmeta.util.Appender.*
import esmeta.util.Appender
import esmeta.interp.*
import esmeta.editor.sview.SyntacticView

class EmptyValueDomain() extends AbsValueDomain {
  val Bot = Elem()
  val Top = Elem()

  def apply(value: AValue): Elem = Elem()

  def fromBoundedAClos(items: (AClo, Map[Int, Elem])*): Elem = Elem()
  def fromAValues[T <: AValue](kind: AValueKind[T])(items: T*): Elem = Elem()

  def mkAbsComp(name: String, value: Elem, target: Elem): Elem = Elem()

  def findHandler(naming: String => String, kind: String): Elem = Elem()

  case class Elem() extends AbsValueTrait {

    def removeNormal: Elem = this
    def normal: Elem = this
    def isAbruptCompletion: Elem = this

    def getHandler = None

    def unary_! : Elem = this
    def ||(that: Elem): Elem = this
    def &&(that: Elem): Elem = this

    def =^=(that: Elem): Elem = this

    def mul(that: Elem): Elem = this
    def plus(that: Elem): Elem = this
    def min(that: Elem): Elem = this
    def max(that: Elem): Elem = this

    def isCompletion: Elem = this
    def project[T <: AValue](kind: AValueKind[T]): Elem = this
    def getSingle[T <: AValue](kind: AValueKind[T]): Flat[T] = FlatTop

    def getSet[T <: AValue](kind: AValueKind[T]): Set[T] = Set()
    def getBoundedCloSet: Set[(AClo, Map[Int, Elem])] = Set()

    def isAllowTopClo = true
    def setAllowTopClo(b: Boolean) = this
    def isFromAbsNode = false

    // join operator
    def ⊔(that: Elem): Elem = this
    def escaped: Elem = this
    def wrapCompletion: Elem = this

    def ⊑(that: Elem): Boolean = true

    // conversion to string
    def toString(detail: Boolean): String = {
      val app = new Appender
      app >> this
      app.toString
    }

    override def beautify(grammar: Option[esmeta.spec.Grammar]): String = {
      "Elem "
    }
  }

  // appender
  implicit val app: Rule[Elem] = (app, elem) => app >> "Elem "

}
