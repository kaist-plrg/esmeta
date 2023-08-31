package esmeta.es.util.fuzzer

import esmeta.BASE_DIR
import esmeta.es.Script
import esmeta.es.util.{JsonProtocol, Coverage, USE_STRICT, cfg}
import esmeta.es.util.Coverage.Interp
import esmeta.es.util.fuzzer.FSTrie.numFeatures
import esmeta.util.SystemUtils.{listFiles, readFile, readJson}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.PriorityQueue as PQueue

object FSTrie {
  val numFeatures = 2104
  implicit val fsOrdering: Ordering[FSData] = Ordering.by(_.value.touch)

  def root: FSTrie = FSTrie(value = FSValue(leaf = false))

  def fromBugs(bugs: List[String]): FSTrie =
    var trie = root
    bugs.foreach(bug =>
      val initSt = cfg.init.from(bug)
      val interp = Interp(initSt, None, false)
      interp.result
      val touchedRawStack = interp.touchedNodeViews.keys
        .flatMap(_.view)
        .map(v => v._2 :: v._1)
        .map(_.map(_.func.name))
        .toSet
      touchedRawStack.foreach(stack => trie = trie.incTouch(stack, Some(true))),
    )
    trie.unleafifyMultipleTouches

  def makeOptimalTrie: FSTrie =
    val fileList = listFiles(s"$BASE_DIR/reported-bugs")
    val cov = Coverage(onlineNumStdDev = Some(1))
    for {
      bugCode <- fileList
      name = bugCode.getName
      code = USE_STRICT + readFile(bugCode.getPath).trim()
      script = Script(code, name)
    } {
      cov.runAndCheck(script)
    }
    cov.getTrie.unleafifyMultipleTouches

  def intersection(trie1: FSTrie, trie2: FSTrie): Int =
    if trie1.value.leaf || trie2.value.leaf then 0
    else
      val keySet1 = trie1.children.keySet
      val keySet2 = trie2.children.keySet
      keySet1.intersect(keySet2).foldLeft(numFeatures) {
        case (acc, key) =>
          val subInterOpt = for {
            child1 <- trie1.children.get(key)
            child2 <- trie2.children.get(key)
          } yield intersection(child1, child2)
          acc + subInterOpt.getOrElse(0)
      }

  def evaluate(): Unit =
    val jsonProtocol = JsonProtocol(cfg)
    import jsonProtocol.given

    val target = readJson[FSTrie](
//      s"$BASE_DIR/experiment/50hrs/sel-1sig-fs/fuzz/fs_trie.json",
//      s"$BASE_DIR/experiment/50hrs/sel-2sig-fcps/fuzz/fs_trie.json",
//      s"$BASE_DIR/experiment/50hrs/sel-2sig-fs/fuzz/fs_trie.json",
      s"$BASE_DIR/experiment/50hrs/sel-2sig-fcps/fuzz/fs_trie.json",
    ).trim()
    val optimal = makeOptimalTrie.trim()
    println(s"optimal size: ${optimal.coverageSize}")
    println(s"target size: ${target.coverageSize}")
    println(s"2-k size: ${numFeatures * numFeatures}")
    println(s"optimal /\\ target size: ${intersection(target, optimal)}")
    println(s"optimal /\\ 2-k size: ${optimal.kIntersection(2)}")
    println(s"target /\\ 2-k size: ${target.kIntersection(2)}")
    println(
      s"optimal /\\ target /\\ 2-k size: ${intersection(target.trim(2), optimal)}",
    )

  def test(): Unit = {

    var t = FSTrie.root

    val abc = List("A", "B", "C")

    1 to 1 foreach { _ => t = t.incTouch(List("A", "A", "B")) }

    1 to 3 foreach { _ => t = t.incTouch(List("A", "B", "C")) }

    1 to 3 foreach { _ => t = t.incTouch(List("A", "B", "D")) }

    1 to 1 foreach { _ => t = t.incTouch(List("B", "C", "D")) }

    1 to 1 foreach { _ => t = t.incTouch(List("B", "D", "E")) }

    1 to 6 foreach { _ => t = t.incTouch(List("C", "D", "E")) }

    println(abc.take(t.getViewLength(abc)))
    println("______________________________")

    t = t.splitMax(Some(1))

    println(abc.take(t.getViewLength(abc)))
    println("______________________________")

    t = t.splitMax(Some(1))

    println(abc.take(t.getViewLength(abc)))
    println("______________________________")

    //    1 to 3 foreach { _ => t = t.incTouch(List("A", "C", "D")) }
    //
    //    println(abc.take(t.getViewLength(abc)))
    //    println(t.trim())
    //    println(abc.take(t.getViewLength(abc)))
    //    t = t.splitMax
    //    println(t.trim())
    //    println("______________________________")
    //    t = t.splitMax
    //    println(abc.take(t.getViewLength(abc)))
    //    println(t.trim())
    //    println("______________________________")
    //    t = t.splitMax
    //    println(abc.take(t.getViewLength(abc)))
    //    println(t.trim())
    //    println("______________________________")
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

  def incTouch(path: List[String], isBugOpt: Option[Boolean] = None): FSTrie =
    incTouchSuppl(path, isBugOpt)

  def getViewLength(stack: List[String]): Int = getViewLengthSuppl(stack, 0)

  def splitMax(numStdDevOpt: Option[Int] = None): FSTrie = {
    val (touchAvg, touchStd) = leafStat
    val threshold =
      touchAvg + touchStd * math.pow(2, -numStdDevOpt.getOrElse(0))
    var targetOpt: Option[FSData] = None
    val pq = collect()
    if (pq.isEmpty) {
      println("FSTrie: result of collect() is empty")
      this
    } else {
      while ({
        val max = pq.dequeue()
        // ensure that the path we are trying to split can be split
        if (max.value.touch > threshold && isTarget(max.path)) {
          targetOpt = Some(max)
        }
        max.value.touch > threshold && targetOpt.isEmpty && pq.nonEmpty
      }) ()
      targetOpt match {
        case None =>
//          println(
//            s"did not split",
//          )
          this
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

  def trim(k: Int): FSTrie = {
    if k == 0 then
      this.copy(value = value.copy(leaf = true), children = Map.empty)
    else if value.leaf then this.copy(children = Map.empty)
    else
      this.copy(
        children = children.transform {
          case (_, child) => child.trim(k - 1)
        },
      )
  }

  def extend(): FSTrie = {
    if children.isEmpty then this
    else
      this.copy(
        children = children.transform {
          case (_, child) => child.extend()
        },
        value = value.copy(leaf = false),
      )
  }

  def leafStat: (Double, Double) = {
    val LeafStat(num, sqSum) = leafStatSuppl
    val avg = value.touch.toDouble / num.toDouble
    val sqAvg = sqSum.toDouble / num.toDouble
    val std = scala.math.sqrt(sqAvg - avg * avg)
    (avg, std)
  }

  def unleafifyMultipleTouches: FSTrie = {
    if value.touch > 1 then
      this.copy(
        children = children.transform {
          case (_, child) => child.unleafifyMultipleTouches
        },
        value = value.copy(leaf = false),
      )
    else
      this.copy(
        children = children.transform {
          case (_, child) => child.unleafifyMultipleTouches
        },
      )
  }

  def kIntersection(k: Int): Int =
    if value.leaf || k <= 0 then 0
    else
      children.values.foldLeft(numFeatures) {
        case (acc, child) => acc + child.kIntersection(k - 1)
      }

  def coverageSize: Int =
    if value.leaf then 0
    else
      children.values.foldLeft(numFeatures) {
        case (acc, child) => acc + child.coverageSize
      }

  private def leafStatSuppl: LeafStat = {
    if value.leaf then {
      LeafStat(num = 1, sqSum = value.touch * value.touch)
    } else {
      children.foldLeft(LeafStat(0, 0)) {
        case (LeafStat(num, sqSum), (_, child)) =>
          val LeafStat(childNum, childSqSum) = child.leafStatSuppl
          LeafStat(num + childNum, sqSum + childSqSum)
      }
    }
  }

  @tailrec
  private def isTarget(path: List[String]): Boolean = path match {
    case Nil => value.leaf && children.nonEmpty
    case head :: tail => // cannot use both @tailrec and flatMap at once
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

  private def incTouchSuppl(
    path: List[String],
    isBugOpt: Option[Boolean],
  ): FSTrie = path match {
    case Nil => this.copy(value = value.incTouch(isBugOpt))
    case head :: tail =>
      FSTrie(
        children.updated(
          head,
          children.getOrElse(head, FSTrie()).incTouchSuppl(tail, isBugOpt),
        ),
        value.incTouch(isBugOpt),
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

case class FSValue(
  touch: Int = 0,
  leaf: Boolean = true,
  bugTouch: Int = 0,
  normalTouch: Int = 0,
) {
  def incTouch(isBugOpt: Option[Boolean]): FSValue =
    isBugOpt match
      case None        => this.copy(touch = touch + 1)
      case Some(true)  => this.copy(bugTouch = bugTouch + 1)
      case Some(false) => this.copy(normalTouch = normalTouch + 1)

  def unleafify: FSValue = this.copy(leaf = false)
}

case class FSData(path: List[String], value: FSValue)

case class LeafStat(num: Int, sqSum: Int)
