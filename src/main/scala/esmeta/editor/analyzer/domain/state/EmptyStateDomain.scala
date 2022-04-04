package esmeta.editor.analyzer

import esmeta.editor.analyzer.*
import esmeta.util.Appender.*
import esmeta.util.Appender
import esmeta.ir.{Id, Global, Local}
import esmeta.js.Initialize
import esmeta.cfg.CFG
import esmeta.ir.Type
import esmeta.interp.Str
import esmeta.js.Syntactic
import esmeta.js.Lexical
import esmeta.editor.util.CFGHelper
import esmeta.editor.sview
import esmeta.editor.sview.SyntacticView
import esmeta.interp.LiteralValue
import esmeta.interp.Absent
import esmeta.interp.*
import esmeta.js

class EmptyStateDomain[AOD <: AbsObjDomain[_] with Singleton](
  aod_ : AOD,
  cfgHelper_ : CFGHelper,
) extends AbsStateDomain[AOD](aod_, cfgHelper_) {

  type AbsValue = aod.avd.Elem
  import AbsValue.{Elem as _, *}

  val Bot = Elem()
  val Empty = Elem()

  // constructors
  def apply(): Elem = Elem()
  // , globals)

  // extractors
  def unapply(elem: Elem) = Some(())

  case class Elem() extends AbsStateTrait {

    override def isBottom = true
    // singleton checks
    def isSingle: Boolean = false

    // singleton location checks
    def isSingle(loc: Loc): Boolean = false
    // handle calls
    def doCall: Elem = this
    def ⊑(that: Elem): Boolean = true

    // join operator
    def ⊔(that: Elem): Elem = this

    // handle returns (this: return states / to: caller states)
    def doReturn(to: Elem, defs: Iterable[(Id, AbsValue)]): Elem = this
    // garbage collection
    def garbageCollected: Elem = this

    // get reachable locations
    def reachableLocs: Set[Loc] = Set()

    // lookup variable directly
    def directLookup(x: Id): AbsValue = AbsValue.Top

    // getters
    def apply(rv: AbsRefValue, cp: ControlPoint): AbsValue = AbsValue.Top
    def apply(x: Id, cp: ControlPoint): AbsValue = AbsValue.Top
    def apply(base: AbsValue, prop: AbsValue): AbsValue = AbsValue.Top

    def apply(view: SyntacticView, lit: LiteralValue): AbsValue = AbsValue.Top

    def apply(ast: Syntactic, lit: LiteralValue): AbsValue = AbsValue.Top

    // NOT sound
    def apply(loc: Loc): AbsObj = AbsObj.Bot

    // lookup local variables
    def lookupLocal(x: Id): AbsValue = AbsValue.Top
    // lookup global variables
    def lookupGlobal(x: Id): AbsValue = AbsValue.Top

    // setters
    def update(refV: AbsRefValue, value: AbsValue): Elem = this
    def update(id: Id, av: AbsValue): Elem = this
    def update(aloc: AbsValue, prop: AbsValue, value: AbsValue): Elem = this

    // existence checks
    def exists(ref: AbsRefValue): AbsValue = AbsValue.Top

    // delete a property from a map
    def delete(refV: AbsRefValue): Elem = this

    // object operators
    def append(loc: AbsValue, value: AbsValue): Elem = this
    def prepend(loc: AbsValue, value: AbsValue): Elem = this
    def remove(loc: AbsValue, value: AbsValue): Elem = this
    def pop(loc: AbsValue, front: Boolean): (AbsValue, Elem) =
      (AbsValue.Bot, this)

    def copyObj(from: AbsValue)(to: AllocSite): Elem = this
    def keys(loc: AbsValue, intSorted: Boolean)(to: AllocSite): Elem = this
    def allocMap(ty: Type, pairs: List[(AbsValue, AbsValue)])(
      to: AllocSite,
    ): Elem = this

    def allocSymbol(desc: AbsValue)(to: AllocSite): Elem = this
    def allocList(list: List[AbsValue])(to: AllocSite): Elem = this
    def setType(loc: AbsValue, ty: Type): Elem = this
    def contains(loc: AbsValue, value: AbsValue): AbsValue = AbsValue.Top

    // define global variables
    def defineGlobal(pairs: (Id, AbsValue)*): Elem = this

    // define local variables
    def defineLocal(pairs: (Id, AbsValue)*): Elem = this
    def replaceLocal(pairs: (Id, AbsValue)*): Elem = this

    // conversion to string
    def toString(detail: Boolean): String =
      val app = new Appender
      app >> this
      app.toString

    // get string wth detailed shapes of locations
    def getString(value: AbsValue): String = "Bot"

  }

  // appender
  implicit val app: Rule[Elem] = (app, elem) => app >> "Bot" >> " "

}
