package esmeta.editor.sview

import esmeta.editor.util.CFGHelper
import esmeta.spec.Production
import esmeta.spec.Lhs
import esmeta.spec.Rhs

class BasicSyntacticView(cfgHelper: CFGHelper) {
  // name, k, AST representation
  private def initAAST(
    name: String,
    paramLen: Int,
    rhsIdx: Int,
    rhs: Rhs,
  ): List[Syntactic] = {
    val xs = for {
      symbol <- rhs.symbols
      nt <- symbol.getNt
    } yield nt
    val initList = List[List[Option[SyntacticView]]](List())
    xs.foldLeft(initList) {
      case (sl, nt) =>
        sl.flatMap(s =>
          if (nt.optional) List(s :+ Some(AbsSyntactic(nt.name)), s :+ None)
          else List(s :+ Some(AbsSyntactic(nt.name))),
        )
    }.filter(_.length > 1)
      .map(s => Syntactic(name, List.fill(paramLen)(false), rhsIdx, s))
  }

  private def injectAAST(lhs: Lhs, rhsIdx: Int, rhs: Rhs, ast: Syntactic) = {
    val xs = for {
      symbol <- rhs.symbols
      nt <- symbol.getNt
    } yield nt
    if (xs.length == 1 && xs(0).name == ast.name && xs(0).optional == false) {
      List(
        Syntactic(
          lhs.name,
          List.fill(lhs.params.length)(false),
          rhsIdx,
          List(Some(ast)),
        ),
      )
    } else {
      List()
    }
  }

  private def nextAAST(ast: Syntactic): (Boolean, List[Syntactic]) = {
    val fname =
      s"${ast.name}[${ast.idx},${cfgHelper.getSubIdxView(ast)}].Evaluation"
    if (cfgHelper.cfg.fnameMap contains fname) {
      (true, Nil)
    } else {
      (
        false,
        cfgHelper.cfg.grammar.prods.flatMap {
          case Production(lhs, Production.Kind.Syntactic, _, rhsList) =>
            rhsList.zipWithIndex.flatMap {
              case (rhs, k) => injectAAST(lhs, k, rhs, ast)
            }
          case _ => List()
        },
      )
    }

  }

  def aux(l: List[Syntactic], r: List[Syntactic]): List[Syntactic] = l match {
    case head :: next =>
      nextAAST(head) match {
        case (true, xs)  => aux(next ++ xs, r :+ head)
        case (false, xs) => aux(next ++ xs, r)
      }
    case Nil => r
  }

  lazy val viewSet: List[Syntactic] =
    val initSeed = cfgHelper.cfg.grammar.prods.flatMap {
      case Production(
            Lhs(name, rawParams),
            Production.Kind.Syntactic,
            _,
            rhsList,
          ) =>
        rhsList.zipWithIndex.flatMap {
          case (rhs, rhsIdx) => initAAST(name, rawParams.length, rhsIdx, rhs)
        }
      case _ => List()
    }
    val fixpoint = aux(initSeed, List())
    fixpoint

}
