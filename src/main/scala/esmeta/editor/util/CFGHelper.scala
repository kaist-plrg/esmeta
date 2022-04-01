package esmeta.editor.util

import esmeta.cfg.CFG
import esmeta.util.BaseUtils.*
import esmeta.cfg.Func
import esmeta.editor.sview.*
import esmeta.js
import esmeta.js.Ast

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
}

object CFGHelper {
  val defaultCases = List(
    "Contains",
    "AllPrivateIdentifiersValid",
  )
}
