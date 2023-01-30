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

object PreFuzzer {
  val ALL = 2
  val PARTIAL = 1
  val NO_DEBUG = 0
  private var futNodeViewCount: Map[Feature, List[(NodeView, Int)]] = Map()
  private var futCondViewCount: Map[Feature, List[(CondView, Int)]] = Map()
  private var nodeViewLowSensScore: Map[NodeView, Double] = Map()
  private var condViewLowSensScore: Map[CondView, Double] = Map()
  private var nodeKMap: Map[String, Int] =
    Map[String, Int]().withDefaultValue(2)
  private var condKMap: Map[String, Int] =
    Map[String, Int]().withDefaultValue(2)

  def preFuzz(
    logInterval: Option[Int] = Some(600), // default is 10 minutes.
    debug: Int = NO_DEBUG, // 2: all, 1: partial, 0: no
    stdOut: Boolean = false,
    timeLimit: Option[Int] = None, // time limitation for each evaluation
    trial: Option[Int] = None, // `None` denotes no bound
    duration: Option[Int] = None, // `None` denotes no bound
    kFs: Int = 0, // feature sensitivity bias
    cp: Boolean = false,
    preFuzzIter: Int = 1,
    attentionPercent: Int = 50,
    cutPercent: Int = 100,
  ): (Map[String, Int], Map[String, Int]) = {
    // TODO: edge selection

    1 to preFuzzIter foreach { iter =>
      println(s"iteration: $iter")
      val (prevSens, currSens, nextSens) = (iter, iter + 1, iter + 2)
      val covPre = Fuzzer(
        logInterval = None,
        debug = debug,
        timeLimit = timeLimit,
        trial = trial,
        duration = duration,
        kFs = kFs,
        cp = cp,
        nodeViewKMap = nodeKMap,
        condViewKMap = condKMap,
      )

      // initialize count and score
      futNodeViewCount = Map()
      futCondViewCount = Map()
      nodeViewLowSensScore = Map()
      condViewLowSensScore = Map()

      // count the edges
      println("######## nodeViewCount")
      covPre.nodeViewCount.take(10).foreach {
        case (nodeView, count) =>
          print(f"$nodeView%200s")
          println(f"  $count%02d")
      }
      countFutNodeView(covPre.nodeViewCount.toList, currSens)
      countFutNodeView(covPre.condViewCount.toList, currSens)

      // score low sensitivity score
      println("######### funtNodeViewCount")
      futNodeViewCount.take(10).foreach {
        case (feature, list) =>
          print(f"$feature%200s")
          println(f"  ${list.size}")
      }
      scoreLowSens(
        futNodeViewCount,
        maxAttentionRatio = attentionPercent / 100.0,
      )
      scoreLowSens(
        futCondViewCount,
        maxAttentionRatio = attentionPercent / 100.0,
      )

      // calculate average scores
      val nodeViewScoreAvg =
        nodeViewLowSensScore.toList
          .map(_._2)
          .sum / nodeViewLowSensScore.size.toDouble
      val condViewScoreAvg =
        condViewLowSensScore.toList
          .map(_._2)
          .sum / condViewLowSensScore.size.toDouble

      // lower the sensitivity if the score is high, otherwise lift it
      val LOW_SENS_CUT_RATIO = cutPercent / 100.0
      println(s"LOW_SENS_CUT_RATIO: $LOW_SENS_CUT_RATIO")
      nodeViewLowSensScore
        .filter(_._2 > nodeViewScoreAvg * LOW_SENS_CUT_RATIO)
        .keys
        .foreach(nodeView =>
          nodeView.view.map {
            case (_, feature, _) =>
              nodeKMap += feature.head.fname -> prevSens
          },
        )
      nodeViewLowSensScore
        .filter(_._2 <= nodeViewScoreAvg * LOW_SENS_CUT_RATIO)
        .keys
        .foreach(nodeView =>
          nodeView.view.map {
            case (_, feature, _) =>
              nodeKMap += feature.head.fname -> nextSens
          },
        )
      condViewLowSensScore
        .filter(_._2 > condViewScoreAvg * LOW_SENS_CUT_RATIO)
        .keys
        .foreach(condView =>
          condView.view.map {
            case (_, feature, _) =>
              condKMap += feature.head.fname -> prevSens
          },
        )
      condViewLowSensScore
        .filter(_._2 <= condViewScoreAvg * LOW_SENS_CUT_RATIO)
        .keys
        .foreach(condView =>
          condView.view.map {
            case (_, feature, _) =>
              condKMap += feature.head.fname -> nextSens
          },
        )
    }
    // return the pre-fuzzing result
    (nodeKMap, condKMap)
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
    maxAttentionRatio: Double,
  ): Unit =
    println(s"MAX_ATTENTION_RATIO: $maxAttentionRatio")
    for ((_, countList) <- futViewCount) yield {
      var sortedCountList = countList.sortBy(_._2).reverse
      val totalCount = countList.map(_._2).sum
      var acc = 0
      while (acc < totalCount * maxAttentionRatio) {
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
    case nodeView: NodeView =>
      nodeView.view
        .map { case (_, f, _) => nodeKMap(f.head.fname) }
        .getOrElse(0)
    case condView: CondView =>
      condView.view
        .map { case (_, f, _) => condKMap(f.head.fname) }
        .getOrElse(0)
  }
}
