package esmeta.editor.sview

import esmeta.editor.util.CFGHelper
import esmeta.spec.Production
import esmeta.spec.Lhs
import esmeta.spec.Rhs
import esmeta.spec.Nonterminal

class BasicSyntacticView(cfgHelper: CFGHelper) {

  lazy val viewSet2: List[Syntactic] =
    val (initEvalSeed, otherSeed, parentMap) = cfgHelper.cfg.grammar.prods
      .foldLeft((Set[String](), Set[String](), Map[String, Set[String]]())) {
        case (
              (s, s2, m),
              Production(lhs, Production.Kind.Syntactic, _, rhsList),
            ) =>
          rhsList.zipWithIndex.foldLeft((s, s2, m)) {
            case ((s, s2, m), (rhs, rhsIdx)) => {
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
                      (if (rhsList.forall((rhs) => rhs.symbols.length == 1))
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
                (cfgHelper.cfg.fnameMap contains
                s"${ast.name}[${ast.idx},${cfgHelper.getSubIdxView(ast)}].Evaluation"),
              )
              val hasOther = nviews.exists((ast) =>
                (cfgHelper.cfg.fnameMap contains
                s"${ast.name}[${ast.idx},${cfgHelper.getSubIdxView(ast)}].PropertyBindingInitialization",
                ) || (cfgHelper.cfg.fnameMap contains
                s"${ast.name}[${ast.idx},${cfgHelper.getSubIdxView(ast)}].RestBindingInitialization",
                ) || (cfgHelper.cfg.fnameMap contains
                s"${ast.name}[${ast.idx},${cfgHelper.getSubIdxView(ast)}].KeyedBindingInitialization",
                ),
              )
              val ns = if (hasEval) s + lhs.name else s
              val ns2 = if (hasOther) s2 + lhs.name else s2
              (ns, ns2, nm)
            }
          }
        case ((s, s2, m), _) => (s, s2, m)
      }
    val (evalSet, otherSet) =
      def aux(i: Set[String]): Set[String] =
        val ni = i ++ (i.map((s) => parentMap.getOrElse(s, Set())).flatten)
        if (ni == i) i else aux(ni)
      (aux(initEvalSeed), aux(otherSeed))

    // val exceptionSet = Set("Elision", "ArgumentList", "ExportsList", "ImportsList", "AssignmentElementList", "AssignmentPropertyList", "BindingPropertyList", "BindingElementList", "BindingRestElement", "ElementList", "FormalParameterList", "PropertySetParameterList", "ClassElementList")

    val initSeed: Map[String, Set[SyntacticView]] =
      cfgHelper.cfg.grammar.prods.flatMap {
        case Production(lhs, Production.Kind.Syntactic, _, _) =>
          List(lhs.name -> Set(AbsSyntactic(lhs.name)))
        case _ => List()
      }.toMap
    def notexists(v: SyntacticView, n: String): Boolean =
      v match
        case Syntactic(name, _, _, children) =>
          name != n && children.forall((v) =>
            v.map((s) => notexists(s, n)).getOrElse(true),
          )
        case _ => v.name != n

    def stepP(
      nm: Map[String, Set[SyntacticView]],
      cyclic: Boolean = false,
    ): Map[String, Set[SyntacticView]] =
      nm.keySet.toList.map {
        case name =>
          cfgHelper.cfg.grammar.prods.find(_.lhs.name == name) match
            case Some(
                  Production(lhs, Production.Kind.Syntactic, _, rhsList),
                )
                if !((evalSet contains lhs.name) || (Set(
                  "BindingPattern",
                  "BindingIdentifier",
                  "BindingRestElement",
                  "BindingElement",
                  "FormalParameters",
                ) contains lhs.name)) => {
              name -> (rhsList.zipWithIndex.toSet.flatMap {
                case (rhs, rhsIdx) =>
                  val xs = for {
                    symbol <- rhs.symbols
                    nt <- symbol.getNt
                  } yield nt
                  if (!cyclic && xs.exists(_.name == name)) List()
                  else
                    val initList = List[List[Option[SyntacticView]]](List())
                    xs.foldLeft(initList) {
                      case (sl, nt) =>
                        val ns =
                          if (cyclic)
                            nm.get(nt.name)
                              .getOrElse(Set(AbsSyntactic(nt.name)))
                          else
                            nm.get(nt.name)
                              .getOrElse(Set(AbsSyntactic(nt.name)))
                              .filter((v) => notexists(v, name))
                        if (nt.optional)
                          sl.flatMap(s => ns.map(s :+ Some(_)) + (s :+ None),
                          )
                        else sl.flatMap(s => ns.map(s :+ Some(_)))
                    }.map(ls =>
                      Syntactic(
                        lhs.name,
                        List.fill(lhs.params.length)(false),
                        rhsIdx,
                        ls,
                      ),
                    )
              })
            }
            case _ => (name -> Set(AbsSyntactic(name)))
      }.toMap

    def finiP(
      nm: Map[String, Set[SyntacticView]],
    ): Map[String, Set[SyntacticView]] =
      nm.keySet.toList.map {
        case name =>
          cfgHelper.cfg.grammar.prods.find(_.lhs.name == name) match
            case Some(
                  Production(lhs, Production.Kind.Syntactic, _, rhsList),
                ) if (evalSet contains lhs.name) => {
              name -> (rhsList.zipWithIndex.toSet.flatMap {
                case (rhs, rhsIdx) =>
                  val xs = for {
                    symbol <- rhs.symbols
                    nt <- symbol.getNt
                  } yield nt
                  val initList = List[List[Option[SyntacticView]]](List())
                  xs.foldLeft(initList) {
                    case (sl, nt) =>
                      val ns =
                        nm.get(nt.name).getOrElse(Set(AbsSyntactic(nt.name)))
                      if (nt.optional)
                        sl.flatMap(s => ns.map(s :+ Some(_)) + (s :+ None),
                        )
                      else sl.flatMap(s => ns.map(s :+ Some(_)))
                  }.map(ls =>
                    Syntactic(
                      lhs.name,
                      List.fill(lhs.params.length)(false),
                      rhsIdx,
                      ls,
                    ),
                  )
              })
            }
            case _ => (name -> Set())
      }.toMap

    val finalSeed =
      def aux(
        nm: Map[String, Set[SyntacticView]],
      ): Map[String, Set[SyntacticView]] =
        val nnm = stepP(nm)
        // nm.toList.map(_._2).flatten.foreach((s) => println(s.toString(false, false, Some(cfgHelper.cfg.grammar))))
        // nm.map{ case (i, j) => (i, j.size)}.foreach(println(_))
        // println(s"size: ${nm.toList.map(_._2).flatten.length}")

        if (nnm == nm) nm else aux(nnm)
      finiP(stepP(aux(initSeed), cyclic = true))
    /*
    def findDeriv(
      name: String,
      forceUnfold: Boolean = false,
      touchedChain: List[String] = List.empty,
      ignoreDup: Boolean = false,
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
                        val recSyn = {
                          println(touchedChain :+ nt.name);
                          if (touchedChain contains nt.name) {
                            if (ignoreDup) List()
                            else
                              findDeriv(
                                nt.name,
                                false,
                                touchedChain :+ nt.name,
                                true,
                              )
                          } else
                            findDeriv(
                              nt.name,
                              false,
                              touchedChain :+ nt.name,
                              ignoreDup,
                            )
                        }
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
          findDeriv(lhs.name, true, List.empty, true).filter((ast) =>
            cfgHelper.cfg.fnameMap contains s"${ast.name}[${ast.idx},${cfgHelper
              .getSubIdxView(ast)}].Evaluation",
          )
        case _ => List()
      }
      .collect { case n: Syntactic => n }

     */
    finalSeed.toList
      .map(_._2)
      .flatten
      .collect { case n: Syntactic => n }
      .filter((ast) =>
        cfgHelper.cfg.fnameMap contains s"${ast.name}[${ast.idx},${cfgHelper
          .getSubIdxView(ast)}].Evaluation",
      )
      .sortBy(_.toString)
  // viewSet2.map(_.toString(false, false, Some(cfgHelper.cfg.grammar))).foreach(println)
}
