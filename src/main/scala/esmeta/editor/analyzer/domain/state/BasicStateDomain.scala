package esmeta.editor.analyzer

import esmeta.editor.analyzer.*
import esmeta.util.Appender.*
import esmeta.util.Appender
import esmeta.ir.Id
import esmeta.js.Initialize
import esmeta.cfg.CFG
import esmeta.ir.Type

class BasicStateDomain[AOD <: AbsObjDomain[_] with Singleton](
  aod_ : AOD,
  cfg_ : CFG,
) extends AbsStateDomain[AOD](aod_, cfg_) {

  type AbsValue = aod.avd.Elem
  import AbsValue.{Elem as _, *}

  val Bot = Elem(false, Map())
  val Empty = Elem(true, Map())

  // constructors
  def apply(
    reachable: Boolean = true,
    locals: Map[Id, AbsValue] = Map(),
    globals: Map[Id, AbsValue] = Map(),
  ): Elem = Elem(reachable, locals)
  // , globals)

  // extractors
  def unapply(elem: Elem) = Some(
    (
      elem.reachable,
      elem.locals,
      // elem.globals,
    ),
  )

  case class Elem(
    reachable: Boolean,
    locals: Map[Id, AbsValue],
    // globals: Map[Id, AbsValue]
  ) extends AbsStateTrait {

    override def isBottom = !this.reachable
    // singleton checks
    def isSingle: Boolean = false

    // singleton location checks
    def isSingle(loc: Loc): Boolean = false
    // handle calls
    def doCall: Elem = this
    def ⊑(that: Elem): Boolean = (this, that) match {
      case _ if this.isBottom => true
      case _ if that.isBottom => false
      case (
            Elem(_, llocals), // , lglobals),
            Elem(_, rlocals), // , rglobals)
          ) => {
        /*
        val localsB = (llocals.keySet ++ rlocals.keySet).forall(x => {
          this.lookupLocal(x) ⊑ that.lookupLocal(x)
        })
        val globalsB = (lglobals.keySet ++ rglobals.keySet).forall(x => {
          this.lookupGlobal(x) ⊑ that.lookupGlobal(x)
        })
        val heapB = lheap ⊑ rheap
        localsB && globalsB && heapB
         */
        val localsB = (llocals.keySet ++ rlocals.keySet).forall(x => {
          this.lookupLocal(x) ⊑ that.lookupLocal(x)
        })
        localsB

      }
    }

    // join operator
    def ⊔(that: Elem): Elem = this

    // handle returns (this: return states / to: caller states)
    def doReturn(to: Elem, defs: Iterable[(Id, AbsValue)]): Elem = Elem(
      reachable = true,
      locals = to.locals ++ defs,
    )

    // garbage collection
    def garbageCollected: Elem = this

    // get reachable locations
    def reachableLocs: Set[Loc] = Set()

    // lookup variable directly
    def directLookup(x: Id): AbsValue = AbsValue.Bot

    // getters
    def apply(rv: AbsRefValue, cp: ControlPoint): AbsValue = AbsValue.Bot
    def apply(x: Id, cp: ControlPoint): AbsValue = AbsValue.Bot
    def apply(base: AbsValue, prop: AbsValue): AbsValue = AbsValue.Bot

    // NOT sound
    def apply(loc: Loc): AbsObj = AbsObj.Bot

    // lookup local variables
    def lookupLocal(x: Id): AbsValue = this match {
      case Elem(_, locals) =>
        locals.getOrElse(x, AbsValue.Bot)
    }
    // lookup global variables
    def lookupGlobal(x: Id): AbsValue = AbsValue.Bot

    // setters
    def update(refV: AbsRefValue, value: AbsValue): Elem = this
    def update(id: Id, av: AbsValue): Elem = this
    def update(aloc: AbsValue, prop: AbsValue, value: AbsValue): Elem = this

    // existence checks
    def exists(ref: AbsRefValue): AbsValue = ???

    // delete a property from a map
    def delete(refV: AbsRefValue): Elem = this

    // object operators
    def append(loc: AbsValue, value: AbsValue): Elem = Bot
    def prepend(loc: AbsValue, value: AbsValue): Elem = Bot
    def remove(loc: AbsValue, value: AbsValue): Elem = Bot
    def pop(loc: AbsValue, front: Boolean): (AbsValue, Elem) =
      (AbsValue.Bot, this)

    def copyObj(from: AbsValue)(to: AllocSite): Elem = Bot
    def keys(loc: AbsValue, intSorted: Boolean)(to: AllocSite): Elem = Bot
    def allocMap(ty: Type, pairs: List[(AbsValue, AbsValue)])(
      to: AllocSite,
    ): Elem = Bot

    def allocSymbol(desc: AbsValue)(to: AllocSite): Elem = Bot
    def allocList(list: List[AbsValue])(to: AllocSite): Elem = Bot
    def setType(loc: AbsValue, ty: Type): Elem = Bot
    def contains(loc: AbsValue, value: AbsValue): AbsValue = AbsValue.Bot

    // define global variables
    def defineGlobal(pairs: (Id, AbsValue)*): Elem = Bot

    // define local variables
    def defineLocal(pairs: (Id, AbsValue)*): Elem = Bot
    def replaceLocal(pairs: (Id, AbsValue)*): Elem = Bot

    // conversion to string
    def toString(detail: Boolean): String =
      val app = new Appender
      app >> this
      app.toString

    // get string wth detailed shapes of locations
    def getString(value: AbsValue): String = "TODO"

  }

  // appender
  implicit val app: Rule[Elem] = (app, elem) => app >> "TODO" >> " "

}
