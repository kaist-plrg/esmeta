package esmeta.es.util.fuzzer

import esmeta.{error as _, *}
import esmeta.cfg.{CFG, Node}
import esmeta.es.util.Coverage.{Cond, NodeOrCondView, CondView, NodeView, View}
import esmeta.es.util.{Coverage, withCFG}
import esmeta.es.util.fuzzer.*
import esmeta.state.Feature
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*

class PreFuzzer {
  val ALL = 2
  val PARTIAL = 1
  val NO_DEBUG = 0
  private val MAX_ATTENTION_RATIO = 0.5
  var futNodeViewCount: Map[Feature, List[(NodeView, Int)]] = Map()
  var futCondViewCount: Map[Feature, List[(CondView, Int)]] = Map()
  var nodeViewLowSensScore: Map[NodeView, Double] = Map()
  var condViewLowSensScore: Map[CondView, Double] = Map()

  def apply(
    logInterval: Option[Int] = Some(600), // default is 10 minutes.
    debug: Int = NO_DEBUG, // 2: all, 1: partial, 0: no
    stdOut: Boolean = false,
    timeLimit: Option[Int] = None, // time limitation for each evaluation
    trial: Option[Int] = None, // `None` denotes no bound
    duration: Option[Int] = None, // `None` denotes no bound
    kFs: Int = 0, // feature sensitivity bias
    cp: Boolean = false,
  ): Unit = {
    val covPre = Fuzzer(
      logInterval = logInterval,
      debug = debug,
      timeLimit = timeLimit,
      trial = trial,
      duration = duration,
      kFs = kFs,
      cp = cp,
    )
    // TODO: edge selection

    countFutNodeView(covPre.nodeViewCount.toList)
    countFutNodeView(covPre.condViewCount.toList)

    scoreLowSens(futNodeViewCount)
    scoreLowSens(futCondViewCount)

    //    nodeViewLowSensScore.toList.sortBy(_._2).foreach {}
    //    condViewLowSensScore.toList.sortBy(_._2).foreach {}
  }

  private def countFutNodeView(
    viewCount: List[(NodeOrCondView, Int)],
  ): Unit = {
    for {
      (view, count) <- viewCount;
      (featureStack, _, _) <- view.getView
    } yield {
      if (featureStack.nonEmpty) {
        val last = featureStack.last
        view match {
          case nodeView: NodeView =>
            futNodeViewCount += last -> ((nodeView, count) :: futNodeViewCount
              .getOrElse(last, List.empty))
          case condView: CondView =>
            futCondViewCount += last -> ((condView, count) :: futCondViewCount
              .getOrElse(last, List.empty))
        }
      }
    }
  }

  private def scoreLowSens(
    futViewCount: Map[Feature, List[(NodeOrCondView, Int)]],
  ): Unit =
    for ((_, countList) <- futViewCount) yield {
      var sortedCountList = countList.sortBy(_._2).reverse
      val totalCount = countList.map(_._2).sum
      var acc = 0
      while (acc < totalCount * MAX_ATTENTION_RATIO) {
        sortedCountList match {
          case h :: t =>
            sortedCountList = t
            acc += h._2
            h._1 match {
              case nodeView: NodeView =>
                nodeViewLowSensScore +=
                  nodeView -> (nodeViewLowSensScore
                    .getOrElse(nodeView, 0.0) + h._2 / totalCount)
              case condView: CondView =>
                condViewLowSensScore +=
                  condView -> (condViewLowSensScore
                    .getOrElse(condView, 0.0) + h._2 / totalCount)
            }
          case _ => acc = totalCount
        }
      }
    }
}

//class PreFuzzer(
//  logInterval: Option[Int] = Some(600), // default is 10 minutes.
//  debug: Int = NO_DEBUG, // 2: all, 1: partial, 0: no
//  stdOut: Boolean = false,
//  timeLimit: Option[Int] = None, // time limitation for each evaluation
//  trial: Option[Int] = None, // `None` denotes no bound
//  duration: Option[Int] = None, // `None` denotes no bound
//  kFs: Int = 0, // feature sensitivity bias
//  cp: Boolean = false,
//) extends Fuzzer(
//    logInterval,
//    debug,
//    stdOut,
//    timeLimit,
//    trial,
//    duration,
//    kFs,
//    cp,
//  ) {
//  import PreFuzzer.*
//
//}
