package esmeta.util

import breeze.signal.OptPadding.ValueOpt

import scala.annotation.tailrec

class Trie[K, V](
  children: Map[K, Trie[K, V]] = Map[K, Trie[K, V]]().empty,
  valueOpt: Option[V] = None,
) {
  @tailrec
  final def apply(path: List[K]): Option[V] = path match {
    case Nil => valueOpt
    case head :: tail => // cannot use both @tailrec and flatMap
      children.get(head) match {
        case None        => None
        case Some(child) => child.apply(tail)
      }
  }
  def insert(path: List[K], value: V): Trie[K, V] =
    path match {
      case Nil => Trie(children, Some(value))
      case head :: tail =>
        Trie(
          children
            .updated(
              head,
              children
                .getOrElse(head, new Trie[K, V]())
                .insert(tail, value),
            ),
          valueOpt,
        )
    }

  def find(subPath: List[K]): Set[(List[K], V)] = subPath match {
    case Nil => collect()
    case head :: tail =>
      children
        .get(head)
        .map(_.find(tail).map {
          case (p, v) => (head :: p, v)
        })
        .getOrElse(Set.empty)
  }

  @tailrec
  final def findValues(subPath: List[K]): Set[V] = subPath match {
    case Nil => collectValues()
    case head :: tail =>
      children.get(head) match {
        case None        => Set.empty
        case Some(child) => child.findValues(tail)
      }
  }

  private def collect(): Set[(List[K], V)] =
    val current = valueOpt.map((List.empty[K], _)).toSet
    val childResults = children.values.toSet.flatMap(_.collect())
    current ++ childResults

  private def collectValues(): Set[V] =
    val current = valueOpt.toSet
    val childResults = children.values.toSet.flatMap(_.collectValues())
    current ++ childResults

}
