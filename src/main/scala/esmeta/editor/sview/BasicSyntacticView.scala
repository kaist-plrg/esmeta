package esmeta.editor.sview

import esmeta.editor.util.CFGHelper
import esmeta.spec.Production
import esmeta.spec.Lhs
import esmeta.spec.Rhs
import esmeta.spec.NtArg
import esmeta.spec.{Nonterminal, Terminal, ButNot, ButOnlyIf}

trait Edge {
  val source: (String, List[Boolean])
  val target: (String, List[Boolean])
  val value: Int
  val mkChain: SyntacticView => Syntactic
}

trait Graph {
  val nodes: Set[(String, List[Boolean])]
  val edgeMap: Map[(String, List[Boolean]), Set[Edge]]
}

object MultiTargetDijkstra {
  import math.Ordering.Implicits.seqOrdering

  def apply(
    g: Graph,
    source: (String, List[Boolean]),
  ): Map[(String, List[Boolean]), Set[List[Edge]]] =
    val dist: collection.mutable.Map[(String, List[Boolean]), Int] =
      collection.mutable.Map()
    val prev: collection.mutable.Map[(String, List[Boolean]), Set[Edge]] =
      collection.mutable.Map()
    val q: collection.mutable.TreeSet[(Int, (String, List[Boolean]))] =
      collection.mutable.TreeSet()

    dist(source) = 0
    prev(source) = Set()
    g.nodes.foreach((s) =>
      if s != source then
        dist(s) = 9999
        prev(s) = Set()
      else ()
      q.add((dist(s), s)),
    )

    def aux: Unit =
      if q.isEmpty then ()
      else
        val (du, u) = q.min
        q.remove((du, u))
        val edges = g.edgeMap(u)
        edges.foreach((e) =>
          val alt = dist(u) + e.value
          if alt < dist(e.target) then
            assert(q.remove((dist(e.target), e.target)))
            dist(e.target) = alt
            prev(e.target) = Set(e)
            q.add((alt, e.target))
          else if alt == dist(e.target) then
            prev(e.target) = prev.getOrElse(e.target, Set()) + e
          else (),
        )
        aux

    aux

    def buildPrev(
      s: (String, List[Boolean]),
      avoidSet: Set[(String, List[Boolean])],
    ): Set[List[Edge]] =
      if s == source then Set(List())
      else
        prev(s)
          .filter((v) => !(avoidSet contains v.source))
          .flatMap((e: Edge) =>
            buildPrev(e.source, avoidSet + e.target).map(_ :+ e),
          )

    g.nodes.map((s) => (s, buildPrev(s, Set()))).toMap
}

class BasicSyntacticView(cfgHelper: CFGHelper) {
  val rhsMap: collection.mutable.Map[(String, Int), Rhs] =
    collection.mutable.Map()
  cfgHelper.cfg.grammar.prods.foreach {
    case Production(lhs, _, _, rhsList) =>
      rhsList.zipWithIndex.foreach {
        case (rhs, rhsIdx) => rhsMap((lhs.name, rhsIdx)) = rhs
      }
  }
  val viewSet: Map[String, Syntactic] =
    def countAllSymbol(s: SyntacticView): Int = s match
      case Syntactic(name, _, rhsIdx, child) =>
        child.foldLeft(0) {
          case (x, i) =>
            x + rhsMap((name, rhsIdx)).symbols.filter {
              case _: Terminal =>
                true
              case _ => false
            }.length + i.map(countAllSymbol).getOrElse(0)
        }
      case _ => 1
    def countTerminalSymbol(s: SyntacticView): Int = s match
      case Syntactic(name, _, rhsIdx, child) =>
        child.foldLeft(0) {
          case (x, i) =>
            x + rhsMap((name, rhsIdx)).symbols.filter {
              case _: Terminal =>
                true
              case _ => false
            }.length + i.map(countTerminalSymbol).getOrElse(0)
        }
      case _ => 0
    def syntacticMeasure(s: SyntacticView): (Int, Int) =
      (countAllSymbol(s), countTerminalSymbol(s))
    def boolCombGen(l: Int): List[List[Boolean]] =
      if (l == 0) List(List())
      else
        boolCombGen(l - 1).flatMap((l) => List(true :: l, false :: l))
    val nodes_ = cfgHelper.cfg.grammar.prods.toSet.flatMap {
      case Production(lhs, _, _, rhsList) =>
        boolCombGen(lhs.params.length)
          .map((params) => (lhs.name, params))
          .toSet ++ rhsList.flatMap((rhs) =>
          rhs.symbols.flatMap(
            _.getNt.toList.flatMap((nt) =>
              boolCombGen(nt.args.length).map((params) => (nt.name, params)),
            ),
          ),
        )
    }
    val edges = cfgHelper.cfg.grammar.prods.flatMap {
      case Production(lhs, Production.Kind.Syntactic, _, rhsList) =>
        boolCombGen(lhs.params.length).map((params) =>
          (lhs.name, params) -> rhsList.zipWithIndex.flatMap {
            case (rhs, rhsIdx) =>
              val pass = rhs.condition match
                case Some(cond) =>
                  if (params(lhs.params.indexOf(cond.name)) != cond.pass) false
                  else true
                case _ => true
              if (!pass) List()
              else
                val rawChild =
                  rhs.symbols
                    .flatMap(_.getNt)
                    .map((nt) => Some(AbsSyntactic(nt.name)))

                val rhsSymbolZipWithIndex =
                  rhs.symbols.flatMap(_.getNt).zipWithIndex
                val nlist =
                  if (lhs.name == "PrimaryExpression" && (rhsIdx == 12)) List()
                  else if (lhs.name == "AsyncArrowFunction" && (rhsIdx == 1))
                    rhsSymbolZipWithIndex.tail
                  else
                    rhsSymbolZipWithIndex
                nlist.map {
                  case (nt, ntIdx) =>
                    new Edge {
                      val source = (lhs.name, params)
                      val target = (
                        nt.name,
                        nt.args.map((ntArg) =>
                          if (ntArg.kind == NtArg.Kind.True) true
                          else if (ntArg.kind == NtArg.Kind.False) false
                          else params(lhs.params.indexOf(ntArg.name)),
                        ),
                      )
                      val value = rhs.symbols.filter {
                        case _: ButNot | _: ButOnlyIf | _: Nonterminal |
                            _: Terminal =>
                          true
                        case _ => false
                      }.length - 1 // if (rhs.symbols.filter(_.getNt.isDefined).length == 1) then 0 else 1 // rhs.symbols.filter(_.getNt.isDefined).length - 1

                      val mkChain = (s: SyntacticView) =>
                        Syntactic(
                          lhs.name,
                          params,
                          rhsIdx,
                          ((rawChild take ntIdx) :+ Some(
                            s,
                          )) ++ (rawChild drop (ntIdx + 1)),
                        )
                    }
                }
          }.toSet,
        )
      case Production(lhs, _, _, _) =>
        boolCombGen(lhs.params.length).map((params) =>
          (lhs.name, params) -> Set(),
        )
    }.toMap
    val graph = new Graph {
      val nodes = nodes_
      val edgeMap = edges
    }
    val rawViews: Map[(String, List[Boolean]), List[(String, Syntactic)]] =
      cfgHelper.cfg.grammar.prods
        .filter {
          case Production(_, Production.Kind.Syntactic, _, _) => true
          case _                                              => false
        }
        .flatMap {
          case Production(lhs, _, _, rhsList) =>
            (boolCombGen(lhs.params.length).map((params) =>
              (lhs.name, params) -> rhsList.zipWithIndex.flatMap(
                (rhs, rhsIdx) =>
                  val pass = rhs.condition match
                    case Some(cond) =>
                      if (params(lhs.params.indexOf(cond.name)) != cond.pass)
                        false
                      else true
                    case _ => true
                  if (!pass) List()
                  else
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
                    combination.zipWithIndex.map {
                      case (l, idx) =>
                        (
                          s"${lhs.name}${rhsIdx}_$idx",
                          Syntactic(
                            lhs.name,
                            params,
                            rhsIdx,
                            l,
                          ),
                        )
                    },
              ),
            ))
        }
        .toMap
    val np = MultiTargetDijkstra(graph, ("Script", List()))
    /*rawViews.foreach {
      case (i, j) =>
        j.foreach((s) =>
          println(s.toString(false, false, Some(cfgHelper.cfg.grammar))),
        )
    }*/
    val finalSet = (np.toList
      .flatMap {
        case (s, setEdge) =>
          rawViews
            .getOrElse(s, List[(String, Syntactic)]())
            .flatMap((v) =>
              setEdge.map((e) =>
                (
                  v._1,
                  e.foldRight[Syntactic](v._2) {
                    case (edge, ab) =>
                      val subIdx = cfgHelper.getSubIdxView(ab)
                      if (
                        (cfgHelper.cfg.fnameMap contains (s"${ab.name}[${ab.idx},${subIdx}].Evaluation"))
                      ) ab
                      else
                        edge.mkChain(ab)
                  },
                ),
              ),
            )
      })
      .foldLeft(Map[String, Set[Syntactic]]()) {
        case (m, (s, syn)) => m + (s -> (m.getOrElse(s, Set()) + syn))
      }
    finalSet
      .map {
        case (s, v) =>
          (
            s, {
              val o = v.toList
              val k = o.map(_.removeParamArg)
              v.filter((x) => o.indexOf(x) == k.indexOf(x.removeParamArg))
            }
              .filter(_.getNormal(cfgHelper).isDefined)
              .filter(syntacticMeasure(_) == v.map(syntacticMeasure).min)
              .toList
              .sortBy((syn) =>
                (
                  syn.name,
                  syn.rhsIdx,
                  syn.toString(false, false, Some(cfgHelper.cfg.grammar)),
                ),
              ),
          )
      }
      .flatMap {
        case (s, l) =>
          l.zipWithIndex.map { case (sv, idx) => (s"${s}_$idx", sv) }
      }
      .toMap
//      .map { case (k, v) => (k, v.minBy(syntacticMeasure)) }
//      .flatMap { case (k, v) => v.getNormal(cfgHelper).map((k, _)) }
  // println(np.toList.length)
  // println(finalList.length)
  // finalList.foreach{ case (i, j) => println(s"${i.toString(false, false, Some(cfgHelper.cfg.grammar))} ---- ${j.toString(false, false, Some(cfgHelper.cfg.grammar))}") }
  /* (np.foreach {
      case (s, v) =>
        v.foreach((e) => println(s"$s -> ${e.map(_.source).mkString(" ")}"))
    })*/

}
