package esmeta.editor.sview

import esmeta.editor.util.CFGHelper
import esmeta.spec.Production
import esmeta.spec.Lhs
import esmeta.spec.Rhs
import esmeta.spec.{Nonterminal, Terminal, ButNot, ButOnlyIf}

trait Edge {
  val source: String
  val target: String
  val value: Int
  val mkChain: SyntacticView => Syntactic
}

trait Graph {
  val nodes: Set[String]
  val edgeMap: Map[String, Set[Edge]]
}

object MultiTargetDijkstra {
  def apply(g: Graph, source: String): Map[String, Set[List[Edge]]] =
    val dist: collection.mutable.Map[String, Int] = collection.mutable.Map()
    val prev: collection.mutable.Map[String, Set[Edge]] =
      collection.mutable.Map()
    val q: collection.mutable.TreeSet[(Int, String)] =
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

    def buildPrev(s: String, avoidSet: Set[String]): Set[List[Edge]] =
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
  val viewSet: Set[Syntactic] =
    val nodes_ = cfgHelper.cfg.grammar.prods.toSet.flatMap {
      case Production(lhs, _, _, rhsList) =>
        Set(lhs.name) ++ rhsList.flatMap((rhs) =>
          rhs.symbols.flatMap(_.getNt.map(_.name)),
        )
    }
    val edges = cfgHelper.cfg.grammar.prods.map {
      case Production(lhs, Production.Kind.Syntactic, _, rhsList) =>
        (lhs.name) -> rhsList.zipWithIndex.flatMap {
          case (rhs, rhsIdx) =>
            val rawChild =
              rhs.symbols
                .flatMap(_.getNt)
                .map((nt) => Some(AbsSyntactic(nt.name)))

            rhs.symbols.flatMap(_.getNt).zipWithIndex.map {
              case (nt, ntIdx) =>
                new Edge {
                  val source = lhs.name
                  val target = nt.name
                  val value = rhs.symbols.filter {
                    case _: ButNot | _: ButOnlyIf | _: Nonterminal |
                        _: Terminal =>
                      true
                    case _ => false
                  }.length - 1 // if (rhs.symbols.filter(_.getNt.isDefined).length == 1) then 0 else 1 // rhs.symbols.filter(_.getNt.isDefined).length - 1

                  val mkChain = (s: SyntacticView) =>
                    Syntactic(
                      lhs.name,
                      List.fill(lhs.params.length)(false),
                      rhsIdx,
                      ((rawChild take ntIdx) :+ Some(
                        s,
                      )) ++ (rawChild drop (ntIdx + 1)),
                    )
                }
            }
        }.toSet
      case Production(lhs, _, _, _) => (lhs.name) -> Set()

    }.toMap
    val graph = new Graph {
      val nodes = nodes_
      val edgeMap = edges
    }
    val rawViews: Map[String, List[Syntactic]] = cfgHelper.cfg.grammar.prods
      .filter {
        case Production(_, Production.Kind.Syntactic, _, _) => true
        case _                                              => false
      }
      .map {
        case Production(lhs, _, _, rhsList) =>
          ((lhs.name) -> rhsList.zipWithIndex.map((rhs, rhsIdx) =>
            Syntactic(
              lhs.name,
              List.fill(lhs.params.length)(false),
              rhsIdx,
              rhs.symbols
                .flatMap(_.getNt)
                .map((nt) => Some(AbsSyntactic(nt.name))),
            ),
          ))
      }
      .toMap
    val np = MultiTargetDijkstra(graph, "Script")
    /*rawViews.foreach {
      case (i, j) =>
        j.foreach((s) =>
          println(s.toString(false, false, Some(cfgHelper.cfg.grammar))),
        )
    }*/
    val finalList = (np.toList.flatMap {
      case (s, setEdge) =>
        rawViews
          .getOrElse(s, Set[Syntactic]())
          .flatMap((v) =>
            setEdge.map((e) =>
              (
                v,
                e.foldRight[Syntactic](v) {
                  case (edge, ab) => edge.mkChain(ab)
                },
              ),
            ),
          )
    })
    // finalList.foreach{ case (i, j) => println(s"${i.toString(false, false, Some(cfgHelper.cfg.grammar))} ---- ${j.toString(false, false, Some(cfgHelper.cfg.grammar))}") }
    /*(np.foreach {
      case (s, v) =>
        v.foreach((e) => println(s"$s -> ${e.map(_.source).mkString(" ")}"))
    })*/

    finalList.map(_._2).toSet
}
