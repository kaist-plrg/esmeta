package esmeta.editor.analyzer

import esmeta.editor.analyzer.*
import esmeta.util.Appender.*
import esmeta.util.Appender
import esmeta.ir.{Id, Global, Local}
import esmeta.js.Initialize
import esmeta.js.builtin.{
  UNDEF_TYPE,
  NULL_TYPE,
  BOOL_TYPE,
  STRING_TYPE,
  SYMBOL_TYPE,
  NUMBER_TYPE,
  BIGINT_TYPE,
  OBJECT_TYPE,
}
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
import scala.util.matching.Regex
import esmeta.editor.sview.AbsSyntactic
import esmeta.editor.sview.AAll

class BasicStateDomain[AOD <: AbsObjDomain[_] with Singleton](
  aod_ : AOD,
  cfgHelper_ : CFGHelper,
) extends AbsStateDomain[AOD](aod_, cfgHelper_) {

  type AbsValue = aod.avd.Elem
  import AbsValue.{Elem as _, *}

  val heapFieldCloNameMap: Map[String, Set[String]] = Map(
    ("unaryMinus") -> Set("Number::unaryMinus", "BigInt::unaryMinus"),
    ("bitwiseNOT") -> Set("Number::bitwiseNOT", "BigInt::bitwiseNOT"),
    ("exponentiate") -> Set("Number::exponentiate", "BigInt::exponentiate"),
    ("multiply") -> Set("Number::multiply", "BigInt::multiply"),
    ("divide") -> Set("Number::divide", "BigInt::divide"),
    ("remainder") -> Set("Number::remainder", "BigInt::remainder"),
    ("add") -> Set("Number::add", "BigInt::add"),
    ("subtract") -> Set("Number::subtract", "BigInt::subtract"),
    ("leftShift") -> Set("Number::leftShift", "BigInt::leftShift"),
    ("signedRightShift") -> Set(
      "Number::signedRightShift",
      "BigInt::signedRightShift",
    ),
    ("unsignedRightShift") -> Set(
      "Number::unsignedRightShift",
      "BigInt::unsignedRightShift",
    ),
    ("lessThan") -> Set("Number::lessThan", "BigInt::lessThan"),
    ("equal") -> Set("Number::equal", "BigInt::equal"),
    ("sameValue") -> Set("Number::sameValue", "BigInt::sameValue"),
    ("sameValueZero") -> Set("Number::sameValueZero", "BigInt::sameValueZero"),
    ("bitwiseAND") -> Set("Number::bitwiseAND", "BigInt::bitwiseAND"),
    ("bitwiseXOR") -> Set("Number::bitwiseXOR", "BigInt::bitwiseXOR"),
    ("bitwiseOR") -> Set("Number::bitwiseOR", "BigInt::bitwiseOR"),
    ("toString") -> Set("Number::toString", "BigInt::toString"),
  )

  val objFieldCloNameMap: Map[String, Set[(String, Map[Int, AbsValue])]] =
    cfg.typeModel.infos.toList
      .map {
        case (ty, ti) =>
          ti.methods.map {
            case (name, fname) =>
              (
                name,
                Set(
                  (
                    fname,
                    Map(
                      0 -> AbsValue(
                        if (ty.endsWith("Object")) ObjAllocSite(ty)
                        else RecordAllocSite(ty),
                      ),
                    ),
                  ),
                ),
              )
          }
      }
      .foldLeft(Map[String, Set[(String, Map[Int, AbsValue])]]()) {
        case (m1, m2) =>
          (m1.keySet ++ m2.keySet).map {
            case key =>
              key -> ((m1.get(key), m2.get(key)) match {
                case (Some(p1), Some(p2)) => p1 ++ p2
                case (None, Some(p2))     => p2
                case (Some(p1), None)     => p1
                case (None, None)         => Set()
              })
          }.toMap
      } //  -- Set("Call", "Get", "Set", "DefineOwnProperty", "HasProperty", "Delete")

  val sdoPattern: Regex = """([a-zA-Z]+)\[\d+,\d+].([a-zA-Z]+)""".r
  val sdoCloMap: Map[String, Set[(AClo, Map[Int, AbsValue])]] =
    cfg.grammar.prods.foldLeft(Map[String, Set[(AClo, Map[Int, AbsValue])]]()) {
      case (m, esmeta.spec.Production(lhs, _, _, rhsList)) =>
        rhsList.zipWithIndex.foldLeft(m) {
          case (m, (rhs, rhsIdx)) =>
            val combination = rhs.symbols
              .flatMap(_.getNt)
              .foldLeft(List[List[Option[AbsSyntactic]]](List())) {
                case (sl, nt) =>
                  if (nt.optional)
                    sl.flatMap((l) =>
                      List(l :+ Some(AbsSyntactic(nt.name)), l :+ None),
                    )
                  else sl.map((l) => l :+ Some(AbsSyntactic(nt.name)))
              }
              .map((l) =>
                sview.Syntactic(
                  lhs.name,
                  List.fill(lhs.params.length)(false),
                  rhsIdx,
                  l,
                ),
              )
            combination.foldLeft(m) {
              case (m, syn: sview.Syntactic) =>
                val subIdx = cfgHelper.getSubIdxView(syn)
                cfg.fnameMap.foldLeft(m) {
                  case (m, (k, v)) =>
                    if (
                      k.startsWith(
                        s"${syn.name}[${syn.idx},${subIdx}].",
                      )
                    ) then {
                      val name =
                        k.drop(s"${syn.name}[${syn.idx},${subIdx}].".length)
                      m + (name -> (m.getOrElse(name, Set()) + (
                        (
                          AClo(v, Map()),
                          Map[Int, AbsValue](0 -> AbsValue(ASView(syn))),
                        ),
                      )))
                    } else m
                }
            }
        }
    } + ("Contains" -> Set(
      (AClo(cfg.fnameMap("<DEFAULT>.Contains"), Map()), Map()),
    )) - "Evaluation"

  /*
    (cfg.fnameMap.keySet
      .map(s =>
        (
          s,
          sdoPattern
            .findFirstMatchIn(s)
            .map((k) => (k.group(2), Some(k.group(1)))),
        ),
      ) ++ Set(
      ("<DEFAULT>.Contains", Some(("Contains", None))),
    ))
      .collect { case (s, Some((k, bound))) => (k, (s, bound)) }
      .foldLeft(Map[String, Set[(String, Option[String])]]()) {
        case (m1, (k, v)) =>
          if (m1 contains k) m1 + (k -> (m1(k) + v)) else m1 + (k -> Set(v))
      }
      .map {
        case (k, v) =>
          (
            k,
            v.map({
              case (name, boundopt) =>
                (
                  AClo(
                    cfg.fnameMap(name),
                    cfg
                      .fnameMap(name)
                      .irFunc
                      .params
                      .map { case (p) => (p.lhs, AbsValue.Top) }
                      .toMap,
                  ),
                  boundopt
                    .map(bound =>
                      Map(
                        0 -> AbsValue(
                          ASView(AbsSyntactic(bound, None, AAll, false)),
                        ),
                      ),
                    )
                    .getOrElse(Map()),
                )
            }),
          )
      } - "Evaluation"*/

  val topCloNameSet = Set(
    "Code",
    "Evaluation",
    "SV",
    "TV",
    "TRV",
    "MV",
    "ResumeCont",
    "ReturnCont",
  ) // ++ Set("Call", "Get", "Set", "DefineOwnProperty", "HasProperty", "Delete")

  val fieldCloMap: Map[String, Set[(AClo, Map[Int, AbsValue])]] =
    heapFieldCloNameMap.map {
      case (k, v) =>
        (
          k,
          v.map((name) =>
            (AClo(cfg.fnameMap(name), Map()), Map[Int, AbsValue]()),
          ),
        )
    }
    ++ objFieldCloNameMap.map {
      case (k, v) =>
        (k, v.map { case (s, m) => (AClo(cfg.fnameMap(s), Map()), m) })
    }

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

    override def isBottom = !this.reachable || locals.values.exists(_.isBottom)
    // singleton checks
    def isSingle: Boolean = false

    // singleton location checks
    def isSingle(loc: AllocSite): Boolean = false
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
    def doReturn(to: Elem, defs: Iterable[(Id, AbsValue)]): Elem =
      val mdefs = defs.toMap
      Elem(
        reachable = true,
        locals = (to.locals.keySet ++ mdefs.keySet).toList
          .map(x => {
            x -> to.lookupLocal(x) ⊔ mdefs.getOrElse(x, AbsValue.Bot)
          })
          .toMap,
      )

    // garbage collection
    def garbageCollected: Elem = this

    // get reachable locations
    def reachableLocs: Set[AllocSite] = Set()

    // lookup variable directly
    def directLookup(x: Id): AbsValue = x match {
      case x: Global => lookupGlobal(x)
      case x: Local  => lookupLocal(x)
    }

    // getters
    def apply(rv: AbsRefValue, cp: ControlPoint): AbsValue = rv match {
      case AbsIdValue(id) => directLookup(id)
      case AbsPropValue(base, prop) => {
        // println(s"base: $base, prop: $prop");
        (prop.getSingle(StrKind) match {
          case FlatElem(ALiteral(Str(s))) =>
            base.getSingle(LocKind) match
              case FlatTop | FlatBot =>
                fieldCloMap.get(s) match {
                  case Some(v: Set[(AClo, Map[Int, AbsValue])]) =>
                    AbsValue.fromBoundedAClos(v.toSeq: _*)
                  case None => {
                    val v = apply(base, prop)
                    if (
                      !(v.isAllowTopClo) || (v.isAllowTopClo && v
                        .getSet(CloKind)
                        .isEmpty)
                    ) {
                      sdoCloMap.get(s) match {
                        case Some(v: Set[(AClo, Map[Int, AbsValue])]) =>
                          AbsValue.fromBoundedAClos(
                            (base.getSingle(AstKind) match {
                              case FlatElem(
                                    ASView(sview),
                                  ) =>
                                v.filter((a) =>
                                  cfgHelper.reachableChild
                                    .getOrElse(sview.name, Set())
                                    .exists((cname) =>
                                      a._1.func.name.startsWith(cname + "["),
                                    ),
                                )
                              case _ => v
                            }).toSeq: _*,
                          )
                        case None =>
                          if (topCloNameSet contains s) v.setAllowTopClo()
                          else v
                      }
                    } else if (topCloNameSet contains s) v.setAllowTopClo()
                    else v
                  }
                }
              case FlatElem(Loc(ListAllocSite)) =>
                if (topCloNameSet contains s) AbsValue.Top.setAllowTopClo()
                else AbsValue.Top
              case FlatElem(Loc(ObjAllocSite(ty))) => {
                val methods = cfg.typeModel(ty)
                methods
                  .find { case (name, fname) => name == s }
                  .map((x) => AbsValue(AClo(cfg.fnameMap(x._2), Map())))
                  .getOrElse(
                    if (topCloNameSet contains s) AbsValue.Top.setAllowTopClo()
                    else AbsValue.Top,
                  )
              }
              case FlatElem(Loc(SymbolAllocSite)) =>
                if (topCloNameSet contains s) AbsValue.Top.setAllowTopClo()
                else AbsValue.Top
              case FlatElem(Loc(RecordAllocSite(ty))) => {
                val methods = cfg.typeModel(ty)
                methods
                  .find { case (name, fname) => name == s }
                  .map((x) => AbsValue(AClo(cfg.fnameMap(x._2), Map())))
                  .getOrElse(
                    if (topCloNameSet contains s) AbsValue.Top.setAllowTopClo()
                    else AbsValue.Top,
                  )
              }
          case _ => apply(base, prop)
        })
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
        case (_, _) =>
          if (base.isAllowTopClo) AbsValue.Top.setAllowTopClo()
          else AbsValue.Top
      }

    // NOT sound
    def apply(loc: AllocSite): AbsObj = loc match
      case ObjAllocSite(ty)    => AbsObj(ty)
      case ListAllocSite       => AbsObj("List")
      case SymbolAllocSite     => AbsObj("Symbol")
      case RecordAllocSite(ty) => AbsObj(ty)

    // lookup local variables
    def lookupLocal(x: Id): AbsValue = this match {
      case Elem(_, locals) =>
        locals.getOrElse(x, AbsValue.Bot)
    }
    // lookup global variables
    def lookupGlobal(x: Id): AbsValue = x match
      case x: Global =>
        x.name match
          case UNDEF_TYPE  => AbsValue(Str("Undefined"))
          case NULL_TYPE   => AbsValue(Str("Null"))
          case BOOL_TYPE   => AbsValue(Str("Boolean"))
          case STRING_TYPE => AbsValue(Str("String"))
          case SYMBOL_TYPE => AbsValue(Str("Symbol"))
          case NUMBER_TYPE => AbsValue(Str("Number"))
          case BIGINT_TYPE => AbsValue(Str("BigInt"))
          case OBJECT_TYPE => AbsValue(Str("Object"))
          case _           => AbsValue.Top
      case _ => AbsValue.Top

    // setters
    def update(refV: AbsRefValue, value: AbsValue): Elem = refV match
      case AbsIdValue(id: Local) => Elem(reachable, locals + (id -> value))
      case _                     => this

    def update(id: Id, av: AbsValue): Elem = id match {
      case id: Id => Elem(reachable, locals + (id -> av))
    }
    def update(aloc: AbsValue, prop: AbsValue, value: AbsValue): Elem = this

    // existence checks
    def existsLocal(x: Id): AbsValue = if (!locals.contains(x))
      AbsValue(ALiteral(Bool(false)))
    else !(lookupLocal(x) =^= AbsValue(ALiteral(Absent)))

    def exists(ref: AbsRefValue): AbsValue = ref match
      case AbsIdValue(id: Local)  => existsLocal(id)
      case AbsIdValue(id: Global) => AbsValue.Top
      case AbsPropValue(base, prop) =>
        !(apply(base.escaped, prop) =^= AbsValue(ALiteral(Absent)))

    // delete a property from a map
    def delete(refV: AbsRefValue): Elem = this

    // object operators
    def append(loc: AbsValue, value: AbsValue): Elem = this
    def prepend(loc: AbsValue, value: AbsValue): Elem = this
    def remove(loc: AbsValue, value: AbsValue): Elem = this
    def pop(loc: AbsValue, front: Boolean): (AbsValue, Elem) =
      (AbsValue.Bot, this)

    def copyObj(from: AllocSite)(to: AllocSite): Elem = this
    def keys(loc: AllocSite, intSorted: Boolean)(to: AllocSite): Elem = this
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

    override def beautify(grammar: Option[esmeta.spec.Grammar]): String =
      toString(false)

  }

  // appender
  implicit val app: Rule[Elem] = (app, elem) =>
    val (topElems, nonTopElems) = elem.locals.toList.partition {
      case (id, value) => AbsValue.Top ⊑ value
    }
    app >> elem.reachable >> " " >> nonTopElems
      .sortWith { case (a, b) => a._1.toString < b._1.toString }
      .map(kv => s"${kv._1} -> ${kv._2.beautify(grammar = Some(cfg.grammar))}")
      .mkString("{", ", ", "}") >>
    topElems
      .sortWith { case (a, b) => a._1.toString < b._1.toString }
      .map(kv => kv._1.toString)
      .mkString(" TOP[", ",", "]") >> " "

}
