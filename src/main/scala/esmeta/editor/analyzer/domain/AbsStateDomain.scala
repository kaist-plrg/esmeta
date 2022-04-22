package esmeta.editor.analyzer

import esmeta.ir.{Type, Id}
import esmeta.js.util.ESValueParser
import esmeta.js.{Lexical, Syntactic, Ast}
import esmeta.cfg.{CFG, Func}
import esmeta.interp.{
  AstValue,
  PureValue,
  Bool,
  Str,
  LiteralValue,
  Absent,
  Math,
}
import esmeta.util.BaseUtils.*
import esmeta.util.StateMonad
import esmeta.error.{NotSupported, InvalidAstProp}
import esmeta.editor.util.CFGHelper
import esmeta.editor.sview.SyntacticView
import esmeta.editor.sview

trait AbsStateDomain[AOD <: AbsObjDomain[_] with Singleton](
  val aod: AOD,
  val cfgHelper: CFGHelper,
) extends Domain {

  val cfg = cfgHelper.cfg
  // monad helper
  val monad: StateMonad[Elem] = new StateMonad[Elem]

  val AbsValue: aod.avd.type = aod.avd
  type AbsValue = AbsValue.Elem

  val AbsObj: aod.type = aod
  type AbsObj = AbsObj.Elem

  import AbsValue.{Elem as _, *}

  val Empty: Elem

  type Elem <: AbsStateTrait

  trait AbsStateTrait extends ElemTrait { this: Elem =>
    // singleton checks
    def isSingle: Boolean

    // singleton location checks
    def isSingle(loc: Loc): Boolean
    // handle calls
    def doCall: Elem

    // handle returns (this: return states / to: caller states)
    def doReturn(to: Elem, defs: (Id, AbsValue)*): Elem = doReturn(to, defs)
    def doReturn(to: Elem, defs: Iterable[(Id, AbsValue)]): Elem

    // garbage collection
    def garbageCollected: Elem

    // get reachable locations
    def reachableLocs: Set[Loc]

    // lookup variable directly
    def directLookup(x: Id): AbsValue

    // getters
    def apply(rv: AbsRefValue, cp: ControlPoint): AbsValue
    def apply(x: Id, cp: ControlPoint): AbsValue
    def apply(base: AbsValue, prop: AbsValue): AbsValue
    def apply(loc: Loc): AbsObj

    /** syntactic SDO */
    case class SyntacticCalled(v: AbsValue, sdo: Func) extends Throwable
    def apply(syn: Syntactic, propStr: String): AbsValue =
      cfgHelper.getSDO((syn, propStr)) match
        case Some((ast0, sdo)) =>
          throw SyntacticCalled(AbsValue(AAst(ast0)), sdo)
        case None => // XXX access to child -> handle this in compiler?
          val Syntactic(name, _, rhsIdx, children) = syn
          val rhs = cfg.grammar.nameMap(name).rhsList(rhsIdx)
          rhs.getNtIndex(propStr).flatMap(children(_)) match
            case Some(child) => AbsValue(AstValue(child))
            case _           => throw InvalidAstProp(syn, Str(propStr))

    def apply(syn: sview.Syntactic, propStr: String): AbsValue =
      cfgHelper.getSDOView((syn, propStr)) match
        case Some((ast0, sdo)) =>
          throw SyntacticCalled(AbsValue(ASView(ast0)), sdo)
        case None => // XXX access to child -> handle this in compiler?
          if (propStr == "Evaluation") syn.chains.last match {
            case sview.AbsSyntactic(_, _, annotation, _) =>
              AbsValue.findHandler(annotation.toString)
            case _ => AbsValue.Top
          }
          else
            syn match {
              case sview.Syntactic(name, _, rhsIdx, children) => {
                val rhs = cfg.grammar.nameMap(name).rhsList(rhsIdx)
                rhs.getNtIndex(propStr).flatMap(children(_)) match
                  case Some(child) => AbsValue(ASView(child))
                  case _           => AbsValue.Top

              }
            }

    /** lexical SDO */
    case class LexicalCalled(value: PureValue) extends Throwable
    def apply(lex: Lexical, sdoName: String): PureValue =
      val Lexical(name, str) = lex
      (name, sdoName) match {
        case (
              "IdentifierName \\ (ReservedWord)" | "IdentifierName",
              "StringValue",
            ) =>
          Str(ESValueParser.parseIdentifier(str))
        // TODO handle numeric seperator in ESValueParser
        case ("NumericLiteral", "MV" | "NumericValue") =>
          ESValueParser.parseNumber(str.replaceAll("_", ""))
        case ("StringLiteral", "SV" | "StringValue") =>
          Str(ESValueParser.parseString(str))
        case ("NoSubstitutionTemplate", "TV") =>
          Str(ESValueParser.parseTVNoSubstitutionTemplate(str))
        case ("TemplateHead", "TV") =>
          Str(ESValueParser.parseTVTemplateHead(str))
        case ("TemplateMiddle", "TV") =>
          Str(ESValueParser.parseTVTemplateMiddle(str))
        case ("TemplateTail", "TV") =>
          Str(ESValueParser.parseTVTemplateTail(str))
        case ("NoSubstitutionTemplate", "TRV") =>
          Str(ESValueParser.parseTRVNoSubstitutionTemplate(str))
        case ("TemplateHead", "TRV") =>
          Str(ESValueParser.parseTRVTemplateHead(str))
        case ("TemplateMiddle", "TRV") =>
          Str(ESValueParser.parseTRVTemplateMiddle(str))
        case ("TemplateTail", "TRV") =>
          Str(ESValueParser.parseTRVTemplateTail(str))
        case (_, "Contains") => Bool(false)
        case ("RegularExpressionLiteral", name) =>
          throw NotSupported(s"RegularExpressionLiteral.$sdoName")
        case _ => error(s"invalid Lexical access: $name.$sdoName")
      }

    def apply(view: SyntacticView, lit: LiteralValue): AbsValue = view match
      case syn: sview.Syntactic =>
        lit match
          case Str("parent") =>
            syn.parent
              .map((x) => AbsValue(ASView(x)))
              .getOrElse(AbsValue(ALiteral(Absent)))
          case Str("children") =>
            AbsValue.Top.setAllowTopClo()
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
        else throw LexicalCalled(apply(Lexical(lex.name, lex.str), propStr))
      case abs: sview.AbsSyntactic =>
        lit match
          case Str("parent") =>
            view.parent
              .map((x) => AbsValue(ASView(x)))
              .getOrElse(AbsValue(ALiteral(Absent)))
          case Str("Evaluation") =>
            AbsValue.findHandler(abs.annotation.toString)
          case _ => AbsValue.Top.setAllowTopClo()

    def apply(ast: Syntactic, lit: LiteralValue): AbsValue = lit match
      case Str("parent") =>
        ast.parent
          .map((x) => AbsValue(AAst(x)))
          .getOrElse(AbsValue(ALiteral(Absent)))
      case Str("children") =>
        AbsValue.Top.setAllowTopClo()
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

    // lookup local variables
    def lookupLocal(x: Id): AbsValue

    // lookup global variables
    def lookupGlobal(x: Id): AbsValue

    // setters
    def update(refV: AbsRefValue, value: AbsValue): Elem
    def update(id: Id, av: AbsValue): Elem
    def update(aloc: AbsValue, prop: AbsValue, value: AbsValue): Elem

    // existence checks
    def exists(ref: AbsRefValue): AbsValue

    // delete a property from a map
    def delete(refV: AbsRefValue): Elem

    // object operators
    def append(loc: AbsValue, value: AbsValue): Elem
    def prepend(loc: AbsValue, value: AbsValue): Elem
    def remove(loc: AbsValue, value: AbsValue): Elem
    def pop(loc: AbsValue, front: Boolean): (AbsValue, Elem)

    def copyObj(from: AbsValue)(to: AllocSite): Elem
    def keys(loc: AbsValue, intSorted: Boolean)(to: AllocSite): Elem
    def allocMap(ty: Type, pairs: List[(AbsValue, AbsValue)])(
      to: AllocSite,
    ): Elem

    def allocSymbol(desc: AbsValue)(to: AllocSite): Elem
    def allocList(list: List[AbsValue])(to: AllocSite): Elem
    def alloc(obj: AbsObj)(to: AllocSite): Elem
    def setType(loc: AbsValue, ty: Type): Elem
    def contains(loc: AbsValue, value: AbsValue): AbsValue

    // define global variables
    def defineGlobal(pairs: (Id, AbsValue)*): Elem

    // define local variables
    def defineLocal(pairs: (Id, AbsValue)*): Elem
    def replaceLocal(pairs: (Id, AbsValue)*): Elem
    def getLocal: Map[Id, AbsValue]

    // conversion to string
    def toString(detail: Boolean): String

    // get string wth detailed shapes of locations
    def getString(value: AbsValue): String

    // check bottom elements in abstract semantics
    private def bottomCheck(vs: Domain#Elem*)(f: => Elem): Elem =
      bottomCheck(vs)(f)
    private def bottomCheck(
      vs: Iterable[Domain#Elem],
    )(f: => Elem): Elem = {
      if (this.isBottom || vs.exists(_.isBottom)) Bot
      else f
    }
  }

  private val defaultCases = List(
    "Contains",
    "AllPrivateIdentifiersValid",
  )
}
