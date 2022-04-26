package esmeta.editor.util

import esmeta.cfg.CFG
import esmeta.util.BaseUtils.*
import esmeta.cfg.Func
import esmeta.editor.sview.*
import esmeta.js
import esmeta.js.Ast
import esmeta.spec.{Production, Nonterminal}
import scala.util.matching.Regex

case class CFGHelper(cfg: CFG) {

  /** get syntax-directed operation(SDO) */
  val getSDOView =
    cached[(SyntacticView, String), Option[(SyntacticView, Func)]] {
      case (ast, operation) =>
        val fnameMap = cfg.fnameMap
        ast.chains.foldLeft[Option[(SyntacticView, Func)]](None) {
          case (None, ast0) =>
            val subIdx = getSubIdxView(ast0)
            val fname = s"${ast0.name}[${ast0.idx},${subIdx}].$operation"
            fnameMap.get(fname) match
              case Some(sdo) => Some(ast0, sdo)
              case None if CFGHelper.defaultCases contains operation =>
                Some(ast0, fnameMap(s"<DEFAULT>.$operation"))
              case _ => None
          case (res: Some[_], _) => res
        }
    }

  /** get sub index of parsed Ast */
  val getSubIdxView = cached[SyntacticView, Int] {
    case abs: AbsSyntactic => -1
    case lex: Lexical      => 0
    case Syntactic(name, _, rhsIdx, children) =>
      val rhs = cfg.grammar.nameMap(name).rhsList(rhsIdx)
      val optionals = (for {
        (opt, child) <- rhs.nts.map(_.optional) zip children if opt
      } yield !child.isEmpty)
      optionals.reverse.zipWithIndex.foldLeft(0) {
        case (acc, (true, idx)) => acc + scala.math.pow(2, idx).toInt
        case (acc, _)           => acc
      }
  }

  /** get syntax-directed operation(SDO) */
  val getSDO = cached[(Ast, String), Option[(Ast, Func)]] {
    case (ast, operation) =>
      val fnameMap = cfg.fnameMap
      ast.chains.foldLeft[Option[(Ast, Func)]](None) {
        case (None, ast0) =>
          val subIdx = getSubIdx(ast0)
          val fname = s"${ast0.name}[${ast0.idx},${subIdx}].$operation"
          fnameMap.get(fname) match
            case Some(sdo) => Some(ast0, sdo)
            case None if CFGHelper.defaultCases contains operation =>
              Some(ast0, fnameMap(s"<DEFAULT>.$operation"))
            case _ => None
        case (res: Some[_], _) => res
      }
  }

  /** get sub index of parsed Ast */
  val getSubIdx = cached[Ast, Int] {
    case lex: js.Lexical => 0
    case js.Syntactic(name, _, rhsIdx, children) =>
      val rhs = cfg.grammar.nameMap(name).rhsList(rhsIdx)
      val optionals = (for {
        (opt, child) <- rhs.nts.map(_.optional) zip children if opt
      } yield !child.isEmpty)
      optionals.reverse.zipWithIndex.foldLeft(0) {
        case (acc, (true, idx)) => acc + scala.math.pow(2, idx).toInt
        case (acc, _)           => acc
      }
  }

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

  val objFieldCloNameMap: Map[String, Set[String]] = cfg.typeModel.infos.toList
    .map {
      case (_, ti) =>
        ti.methods.map {
          case (name, fname) => (name, Set(fname))
        }
    }
    .foldLeft(Map[String, Set[String]]()) {
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
    }

  val sdoPattern: Regex = """[a-zA-Z]+\[\d+,\d+].([a-zA-Z]+)""".r
  val sdoCloNameMap: Map[String, Set[String]] = cfg.fnameMap.keySet
    .map(s => (s, sdoPattern.findFirstMatchIn(s).map(_.group(1))))
    .collect { case (s, Some(k)) => (k, s) }
    .foldLeft(Map[String, Set[String]]()) {
      case (m1, (k, v)) =>
        if (m1 contains k) m1 + (k -> (m1(k) + v)) else m1 + (k -> Set(v))
    }

  val topCloNameSet = sdoCloNameMap.keySet ++ Set(
    "ResumeCont",
    "ReturnCont",
    "Code",
    "Job",
    "SV",
    "TV",
    "TRV",
    "MV",
    // Modules
    "Link",
    "Evaluate",
    "ParseModule",
    "GetExportedNames",
    "ResolveExport",
    "InitializeEnvironment",
    "ExecuteModule",
    "CreateImportBinding",
    // Memory Model
    "ReadsBytesFrom",
  )
  /*  "Call",
    "Get",
    "Set",
    "DefineOwnProperty",
    "HasProperty",
    "Delete",
  )*/

  val fieldCloMap: Map[String, Set[String]] =
    (heapFieldCloNameMap ++ objFieldCloNameMap) // -- Set("Call", "Get", "Set", "DefineOwnProperty", "HasProperty", "Delete")

  val nameCloMap: Map[String, Set[String]] = fieldCloMap ++ sdoCloNameMap

  val reachableChild: Map[String, Set[String]] =
    val cMap: Map[String, Set[String]] =
      val init = Map[String, Set[String]]()
      cfg.grammar.prods.foldLeft(init) {
        case (m, Production(lhs, _, _, rhsList)) =>
          rhsList.foldLeft(
            m + (lhs.name -> (m.getOrElse(lhs.name, Set()) + lhs.name)),
          ) {
            case (m, rhs) =>
              rhs.symbols.flatMap(_.getNt) match {
                case (x: Nonterminal) :: Nil =>
                  m + (lhs.name -> (m.getOrElse(lhs.name, Set()) + x.name))
                case _ => m
              }
          }
      }
    def aux(x: Map[String, Set[String]]): Map[String, Set[String]] =
      val nx = x.map { case (k, v) => (k, v.flatMap(x.getOrElse(_, Set()))) }
      if (nx == x) nx else aux(nx)
    aux(cMap)

}

object CFGHelper {
  val defaultCases = List(
    "Contains",
    "AllPrivateIdentifiersValid",
  )
}
