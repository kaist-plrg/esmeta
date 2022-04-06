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

class BasicStateDomain[AOD <: AbsObjDomain[_] with Singleton](
  aod_ : AOD,
  cfgHelper_ : CFGHelper,
) extends AbsStateDomain[AOD](aod_, cfgHelper_) {

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
    def ⊔(that: Elem): Elem = {
      Elem(
        reachable = this.reachable || that.reachable,
        locals = (this.locals.keySet ++ that.locals.keySet).toList
          .map(x => {
            x -> this.lookupLocal(x) ⊔ that.lookupLocal(x)
          })
          .toMap,
      )
    }

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
    def directLookup(x: Id): AbsValue = x match {
      case x: Global => lookupGlobal(x)
      case x: Local  => lookupLocal(x)
    }

    // getters
    def apply(rv: AbsRefValue, cp: ControlPoint): AbsValue = rv match {
      case AbsIdValue(id) => directLookup(id)
      case AbsPropValue(base, prop) => {
        apply(base, prop) // println(s"base: $base, prop: $prop");
      }
    }
    def apply(x: Id, cp: ControlPoint): AbsValue = directLookup(x)
    def apply(base: AbsValue, prop: AbsValue): AbsValue =
      (base.getSingle(AstKind), prop.getSingle(LiteralKind)) match {
        case (FlatElem(ASView(view)), FlatElem(ALiteral(a))) => {
          apply(view, a)
        }
        case (FlatElem(AAst(ast: Syntactic)), FlatElem(ALiteral(a))) => {
          apply(ast, a)
        }
        case (FlatElem(AAst(ast: Lexical)), FlatElem(ALiteral(a))) => {
          apply(ast, a)
        }
        case (_, _) => AbsValue.Top
      }

    def apply(view: SyntacticView, lit: LiteralValue): AbsValue = view match
      case syn: sview.Syntactic =>
        lit match
          case Str("parent") =>
            syn.parent
              .map((x) => AbsValue(ASView(x)))
              .getOrElse(AbsValue(ALiteral(Absent)))
          case Str("children") =>
            AbsValue.Top
          case Str(propStr) =>
            apply(syn, propStr)
          case Math(n) if n.isValidInt =>
            syn.children(n.toInt) match
              case Some(child) => AbsValue(ASView(child))
              case None        => AbsValue(ALiteral(Absent))
          case _ => AbsValue.Bot
      case lex: sview.Lexical =>
        val propStr = lit.asStr
        if (propStr == "parent")
          view.parent
            .map((x) => AbsValue(ASView(x)))
            .getOrElse(AbsValue(ALiteral(Absent)))
        else throw LexicalCalled(apply(js.Lexical(lex.name, lex.str), propStr))
      case abs: sview.AbsSyntactic =>
        lit match
          case Str("parent") =>
            view.parent
              .map((x) => AbsValue(ASView(x)))
              .getOrElse(AbsValue(ALiteral(Absent)))
          case _ => AbsValue.Top

    def apply(ast: Syntactic, lit: LiteralValue): AbsValue = lit match
      case Str("parent") =>
        ast.parent
          .map((x) => AbsValue(AAst(x)))
          .getOrElse(AbsValue(ALiteral(Absent)))
      case Str("children") =>
        AbsValue.Top
      case Str(propStr) =>
        apply(ast, propStr)
      case Math(n) if n.isValidInt =>
        ast.children(n.toInt) match
          case Some(child) => AbsValue(AAst(child))
          case None        => AbsValue(ALiteral(Absent))
      case _ => AbsValue.Bot

    def apply(ast: Lexical, lit: LiteralValue): AbsValue =
      val propStr = lit.asStr
      if (propStr == "parent")
        ast.parent
          .map((x) => AbsValue(AAst(ast)))
          .getOrElse(AbsValue(ALiteral(Absent)))
      else throw LexicalCalled(apply(ast, propStr))

    // NOT sound
    def apply(loc: Loc): AbsObj = AbsObj.Bot

    // lookup local variables
    def lookupLocal(x: Id): AbsValue = this match {
      case Elem(_, locals) =>
        locals.getOrElse(x, AbsValue.Bot)
    }
    // lookup global variables
    def lookupGlobal(x: Id): AbsValue = AbsValue.Top

    // setters
    def update(refV: AbsRefValue, value: AbsValue): Elem = this
    def update(id: Id, av: AbsValue): Elem = id match {
      case id: Id => Elem(reachable, locals + (id -> av))
    }
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
    def alloc(obj: AbsObj)(to: AllocSite): Elem = this
    def setType(loc: AbsValue, ty: Type): Elem = this
    def contains(loc: AbsValue, value: AbsValue): AbsValue = AbsValue.Top

    // define global variables
    def defineGlobal(pairs: (Id, AbsValue)*): Elem = this

    // define local variables
    def defineLocal(pairs: (Id, AbsValue)*): Elem =
      this.copy(locals = this.locals ++ pairs.toMap)
    def replaceLocal(pairs: (Id, AbsValue)*): Elem =
      this.copy(locals = pairs.toMap)

    def getLocal = locals

    // conversion to string
    def toString(detail: Boolean): String =
      val app = new Appender
      app >> this
      app.toString

    // get string wth detailed shapes of locations
    def getString(value: AbsValue): String = this.toString

  }

  // appender
  implicit val app: Rule[Elem] = (app, elem) =>
    val (topElems, nonTopElems) = elem.locals.toList.partition {
      case (id, value) => AbsValue.Top ⊑ value
    }
    app >> elem.reachable >> " " >> nonTopElems
      .sortWith { case (a, b) => a._1.toString < b._1.toString }
      .map(kv => s"${kv._1} -> ${kv._2}")
      .mkString("{", ", ", "}") >>
    topElems
      .sortWith { case (a, b) => a._1.toString < b._1.toString }
      .map(kv => kv._1.toString)
      .mkString(" TOP[", ",", "]") >> " "

}
