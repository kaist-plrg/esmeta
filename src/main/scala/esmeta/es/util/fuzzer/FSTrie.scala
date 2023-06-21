package esmeta.es.util.fuzzer

import esmeta.util.Trie

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.PriorityQueue as PQueue

object FSTrie {
  implicit val fsOrdering: Ordering[FSData] = Ordering.by(_.value.touch)

  def root: FSTrie = FSTrie(value = FSValue(leaf = false))

  def test(): Unit = {
    var t = FSTrie.root
    val abc = List("a", "b", "c")
    1 to 4 foreach { _ => t = t.incTouch(List("a", "b", "c")) }
    1 to 3 foreach { _ => t = t.incTouch(List("b", "c")) }
    1 to 3 foreach { _ => t = t.incTouch(List("b", "d")) }
    1 to 2 foreach { _ => t = t.incTouch(List("c", "d")) }

    println(abc.take(t.getViewLength(abc)))
    println(t.trim())
    println("______________________________")
    t = t.splitMax
    println(abc.take(t.getViewLength(abc)))
    println(t.trim())
    println("______________________________")
    t = t.splitMax
    println(abc.take(t.getViewLength(abc)))
    println(t.trim())
    println("______________________________")
    t = t.splitMax
    println(abc.take(t.getViewLength(abc)))
    println(t.trim())
    println("______________________________")
  }
}

case class FSTrie(
  children: Map[String, FSTrie] = Map[String, FSTrie]().empty,
  value: FSValue = FSValue(),
) {

  import FSTrie.fsOrdering

  @tailrec
  final def apply(path: List[String]): FSValue = path match {
    case Nil => value
    case head :: tail => // cannot use both @tailrec and flatMap
      children.get(head) match {
        case None        => FSValue()
        case Some(child) => child.apply(tail)
      }
  }

  def incTouch(path: List[String]): FSTrie =
    incTouchSuppl(path)

  def getViewLength(stack: List[String]): Int = getViewLengthSuppl(stack, 0)

  def splitMax: FSTrie = {
    var targetOpt: Option[FSData] = None
    val pq = collect()
    if (pq.isEmpty) {
      println("FSTrie: result of collect() is empty")
      this
    } else {
      while ({
        val max = pq.dequeue()
        if (isTarget(max.path)) {
          targetOpt = Some(max)
        }
        targetOpt.isEmpty && pq.nonEmpty
      }) ()
      targetOpt match {
        case None => this
        case Some(target) =>
          println(
            s"split: ${target.path.map(_.replaceAll("[aeiou]", "").take(16))}",
          )
          this.unleafify(target.path)
      }
    }
  }

  def trim(): FSTrie = {
    if value.leaf then this.copy(children = Map.empty)
    else
      this.copy(
        children = children.transform {
          case (_, child) => child.trim()
        },
      )
  }

  @tailrec
  private def isTarget(path: List[String]): Boolean = path match {
    case Nil => value.leaf && children.nonEmpty
    case head :: tail => // cannot use both @tailrec and flatMap
      children.get(head) match {
        case None        => false
        case Some(child) => child.isTarget(tail)
      }
  }

  private def unleafify(path: List[String]): FSTrie =
    path match {
      case Nil => FSTrie(children, value.unleafify)
      case head :: tail =>
        FSTrie(
          children.updated(
            head,
            children.getOrElse(head, FSTrie()).unleafify(tail),
          ),
          value.unleafify,
        )
    }

  private def collect(): PQueue[FSData] =
    collectSuppl(List.empty, PQueue[FSData]())

  private def collectSuppl(
    path: List[String],
    pQueue: PQueue[FSData],
  ): PQueue[FSData] =
    if value.leaf then pQueue.addOne(FSData(path, value))
    else
      children.foldLeft(pQueue) {
        case (pq, (key, child)) => child.collectSuppl(path :+ key, pq)
      }

  private def incTouchSuppl(path: List[String]): FSTrie = path match {
    case Nil => FSTrie(children, value.incTouch)
    case head :: tail =>
      FSTrie(
        children.updated(
          head,
          children.getOrElse(head, FSTrie()).incTouchSuppl(tail),
        ),
        value.incTouch,
      )
  }

  private def getViewLengthSuppl(stack: List[String], acc: Int): Int =
    stack match {
      case _ if value.leaf => acc
      case Nil             => acc
      case head :: tail =>
        children
          .get(head)
          .map(_.getViewLengthSuppl(tail, acc + 1))
          .getOrElse(acc)
    }
}

case class FSValue(touch: Int = 0, leaf: Boolean = true) {
  def incTouch: FSValue = this.copy(touch = touch + 1)

  def unleafify: FSValue = this.copy(leaf = false)
}

case class FSData(path: List[String], value: FSValue)
