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
  private val LOW_SENS_CUT_RATIO = 1
  var futNodeViewCount: Map[Feature, List[(NodeView, Int)]] = Map()
  var futCondViewCount: Map[Feature, List[(CondView, Int)]] = Map()
  var nodeViewLowSensScore: Map[NodeView, Double] = Map()
  var condViewLowSensScore: Map[CondView, Double] = Map()
  var nodeViewKMap: Map[Node, Int] = Map[Node, Int]().withDefaultValue(1)
  var condViewKMap: Map[Cond, Int] = Map[Cond, Int]().withDefaultValue(1)

  def apply(
    logInterval: Option[Int] = Some(600), // default is 10 minutes.
    debug: Int = NO_DEBUG, // 2: all, 1: partial, 0: no
    stdOut: Boolean = false,
    timeLimit: Option[Int] = None, // time limitation for each evaluation
    trial: Option[Int] = None, // `None` denotes no bound
    duration: Option[Int] = None, // `None` denotes no bound
    kFs: Int = 0, // feature sensitivity bias
    cp: Boolean = false,
    preFuzzIter: Int = 1,
  ): (Map[Node, Int], Map[Cond, Int]) = {
    // TODO: edge selection

    1 to preFuzzIter foreach { iter =>
      val (prevSens, currSens, nextSens) = (iter - 1, iter, iter + 1)
      val covPre = Fuzzer(
        logInterval = logInterval,
        debug = debug,
        timeLimit = timeLimit,
        trial = trial,
        duration = duration,
        kFs = kFs,
        cp = cp,
        nodeViewKMap = nodeViewKMap,
        condViewKMap = condViewKMap,
      )

      // initialize count and score
      futNodeViewCount = Map()
      futCondViewCount = Map()
      nodeViewLowSensScore = Map()
      condViewLowSensScore = Map()

      // count the edges
      countFutNodeView(covPre.nodeViewCount.toList, currSens)
      countFutNodeView(covPre.condViewCount.toList, currSens)

      // score low sensitivity score
      scoreLowSens(futNodeViewCount)
      scoreLowSens(futCondViewCount)

      // calculate average scores
      val nodeViewScoreAvg =
        nodeViewLowSensScore.toList.map(_._2).sum / nodeViewLowSensScore.size
      val condViewScoreAvg =
        condViewLowSensScore.toList.map(_._2).sum / condViewLowSensScore.size

      // lower the sensitivity if the score is high, otherwise lift it
      nodeViewLowSensScore
        .filter(_._2 > nodeViewScoreAvg * LOW_SENS_CUT_RATIO)
        .keys
        .foreach(nodeView => nodeViewKMap += nodeView.node -> prevSens)
      nodeViewLowSensScore
        .filter(_._2 <= nodeViewScoreAvg * LOW_SENS_CUT_RATIO)
        .keys
        .foreach(nodeView => nodeViewKMap += nodeView.node -> nextSens)
      condViewLowSensScore
        .filter(_._2 > condViewScoreAvg * LOW_SENS_CUT_RATIO)
        .keys
        .foreach(condView => condViewKMap += condView.cond -> prevSens)
      condViewLowSensScore
        .filter(_._2 <= condViewScoreAvg * LOW_SENS_CUT_RATIO)
        .keys
        .foreach(condView => condViewKMap += condView.cond -> nextSens)
    }
    // return the pre-fuzzing result
    (nodeViewKMap, condViewKMap)
  }

  private def countFutNodeView(
    viewCount: List[(NodeOrCondView, Int)],
    currSens: Int,
  ): Unit = {
    for {
      (view, count) <- viewCount;
      (featureStack, _, _) <- view.getView
    } yield {
      if ((getSens(view) == currSens) && featureStack.nonEmpty) {
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
              // TODO: which to add: count or ratio?
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

  private def getSens(view: NodeOrCondView): Int = view match {
    case nodeView: NodeView => nodeViewKMap(nodeView.node)
    case condView: CondView => condViewKMap(condView.cond)
  }
}
