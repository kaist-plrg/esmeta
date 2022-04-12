package esmeta.editor.sview

import esmeta.editor.util.CFGHelper
import esmeta.spec.Production
import esmeta.spec.Lhs
import esmeta.spec.Rhs
import esmeta.spec.Nonterminal

class BasicSyntacticView(cfgHelper: CFGHelper) {

  lazy val viewSet: List[Syntactic] =
    // name, k, AST representation
    def initAAST(
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
    def injectAAST(lhs: Lhs, rhsIdx: Int, rhs: Rhs, ast: Syntactic) = {
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
    def nextAAST(ast: Syntactic): (Boolean, List[Syntactic]) = {
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

  lazy val viewSet2: List[Syntactic] =
    val (initEvalSeed, parentMap) = cfgHelper.cfg.grammar.prods
      .foldLeft((Set[String](), Map[String, Set[String]]())) {
        case ((s, m), Production(lhs, Production.Kind.Syntactic, _, rhsList)) =>
          rhsList.zipWithIndex.foldLeft((s, m)) {
            case ((s, m), (rhs, rhsIdx)) => {
              val xs = for {
                symbol <- rhs.symbols
                nt <- symbol.getNt
              } yield nt
              val (fakeViews, nm) =
                val initList = List[List[Option[SyntacticView]]](List())
                xs.foldLeft((initList, m)) {
                  case ((sl, m), nt) =>
                    (
                      sl.flatMap(s =>
                        if (nt.optional)
                          List(s :+ Some(AbsSyntactic(nt.name)), s :+ None)
                        else List(s :+ Some(AbsSyntactic(nt.name))),
                      ),
                      (if (rhs.symbols.length == 1)
                         m + (nt.name -> (m.getOrElse(
                           nt.name,
                           Set(),
                         ) + lhs.name))
                       else m),
                    )
                }
              val nviews = fakeViews.map(s =>
                Syntactic(
                  lhs.name,
                  List.fill(lhs.params.length)(false),
                  rhsIdx,
                  s,
                ),
              )
              // val tmpSView = Syntactic(lhs.name, List.fill(lhs.params.length)(false), rhsIdx)
              val hasEval = nviews.exists((ast) =>
                (cfgHelper.cfg.fnameMap exists {
                  case (k, _) =>
                    k.startsWith(
                      s"${ast.name}[${ast.idx},${cfgHelper.getSubIdxView(ast)}].",
                    )
                }),
              )
              val ns = if (hasEval) s + lhs.name else s
              (ns, nm)
            }
          }
        case ((s, m), _) => (s, m)
      }
    val evalSet =
      def aux(i: Set[String]): Set[String] =
        val ni = i ++ (i.map((s) => parentMap.getOrElse(s, Set())).flatten)
        if (ni == i) i else aux(ni)
      aux(initEvalSeed)

    // val exceptionSet = Set("Elision", "ArgumentList", "ExportsList", "ImportsList", "AssignmentElementList", "AssignmentPropertyList", "BindingPropertyList", "BindingElementList", "BindingRestElement", "ElementList", "FormalParameterList", "PropertySetParameterList", "ClassElementList")

    def findDeriv(
      name: String,
      forceUnfold: Boolean = false,
    ): List[SyntacticView] = {
      cfgHelper.cfg.grammar.prods.flatMap {
        case Production(lhs, Production.Kind.Syntactic, _, rhsList)
            if lhs.name == name =>
          val hasEval = evalSet contains lhs.name
          if ((!forceUnfold && hasEval)) List(AbsSyntactic(name))
          else {
            rhsList.zipWithIndex.flatMap {
              case (rhs, rhsIdx) => {
                // val tmpSView = Syntactic(lhs.name, List.fill(lhs.params.length)(false), rhsIdx)
                val xs = for {
                  symbol <- rhs.symbols
                  nt <- symbol.getNt
                } yield nt
                val realViews =
                  val initList = List[List[Option[SyntacticView]]](List())
                  xs.foldLeft(initList) {
                    case (sl, nt) =>
                      sl.flatMap(s =>
                        val recSyn = findDeriv(nt.name)
                        if (nt.optional) recSyn.map(s :+ Some(_)) :+ (s :+ None)
                        else recSyn.map(s :+ Some(_)),
                      )
                  }.map(s =>
                    Syntactic(
                      lhs.name,
                      List.fill(lhs.params.length)(false),
                      rhsIdx,
                      s,
                    ),
                  )
                realViews
              }
            }
          }
        case _ => List()
      }
    }
    cfgHelper.cfg.grammar.prods
      .flatMap {
        case Production(lhs, Production.Kind.Syntactic, _, rhsList) =>
          findDeriv(lhs.name, true).filter((ast) =>
            cfgHelper.cfg.fnameMap contains s"${ast.name}[${ast.idx},${cfgHelper
              .getSubIdxView(ast)}].Evaluation",
          )
        case _ => List()
      }
      .collect { case n: Syntactic => n }
  // viewSet2.map(_.toString(false, false, Some(cfgHelper.cfg.grammar))).foreach(println)
}
