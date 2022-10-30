package esmeta.phase

import esmeta.*
import esmeta.cfg.*
import esmeta.es.util.*
import esmeta.es.util.Coverage.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.util.BaseUtils.*
import io.circe.*, io.circe.generic.semiauto.*
import io.circe.syntax._
import scala.collection.mutable.{Map => MMap}

/** `localize` phase */
case object Localize
  extends Phase[CFG, Map[String, Map[String, Seq[(Any, Double)]]]] {
  val name = "localize"
  val help = "Localize the bug"

  type Target = String
  type Test = String
  type Result = Map[Target, Map[Test, Seq[(FuncView, Double)]]]
  type MResult = MMap[Target, Map[Test, Seq[(FuncView, Double)]]]
  type IResult = MMap[Target, Map[Test, Seq[(Int, Double)]]]

  private var _config: Config = null

  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Result = withCFG(cfg) {
    val jsonProtocol = JsonProtocol(cfg)
    import jsonProtocol.given
    _config = config

    // name of json files
    val (fuzzLogDir, failsMapJson) = optional {
      (cmdConfig.targets(0), cmdConfig.targets(1))
    }
      .getOrElse {
        warn(
          "No explicit paths are given. Trying to use the result from the most recent instead..",
        )
        (
          s"$FUZZ_LOG_DIR/recent",
          s"$CONFORMTEST_LOG_DIR/fails.json",
        )
      }
    val nodeViewCoverageJson = s"$fuzzLogDir/node-coverage.json"
    val touchedNodeViewJson = s"$fuzzLogDir/minimal-touch-nodeview.json"

    // parse jsons
    val nodeViewInfos: Vector[NodeViewInfo] =
      readJson(nodeViewCoverageJson)
    val nodeViews = nodeViewInfos.map(_.nodeView)
    val nodeViewIds = nodeViewInfos.map(_.index)

    val funcViews = nodeViews.map(_.toFuncView).distinct
    val funcView2Id = funcViews.zipWithIndex.map(_ -> _).toMap

    val test2NodeViewIds: Map[Test, Vector[Int]] =
      readJson(touchedNodeViewJson)
    val tests = test2NodeViewIds.keys.toVector

    val target2Fails: Map[Target, Set[Test]] = readJson(failsMapJson)

    // iterate all targets and get localization result for each failed tests
    var iresult: IResult = MMap()
    var result: MResult = MMap()
    target2Fails.foreach((target, fails) => {
      if (_config.debug) println(s"Localizing for the target `$target`...")

      val passes = tests.filterNot(fails.contains(_))

      type ScoreMap = Map[Test, IndexedSeq[Double]]
      val nodeViewScores: ScoreMap =
        localize(
          nodeViewIds,
          passes,
          fails,
          test2NodeViewIds,
        )
      val funcViewScores: ScoreMap = {
        nodeViewScores.map(
          _ -> _.zipWithIndex
            .groupMapReduce((score, idx) =>
              funcView2Id(nodeViews(idx).toFuncView),
            )(_._1)(_ max _)
            .foldLeft(funcViews.map(_ => 0.0)) {
              case (scores, (id, score)) =>
                scores.updated(id, score)
            },
        )
      }

      val mergedScores = funcViewScores.map((test, scores) => {
        val funcScores = scores.zipWithIndex.map(_.swap)
        test -> funcScores
      })

      iresult(target) = mergedScores
      result(target) = mergedScores.map((test, indexedScores) => {
        test -> indexedScores
          .sortBy(_._2)
          .reverse
          .slice(0, _config.topN.get)
          .map {
            case (i, score) => (funcViews(i), score)
          }
      })
    })

    dumpJson(result, s"$LOCALIZE_LOG_DIR/localize.json")

    result.toMap
  }

  private def localize(
    ids: IndexedSeq[Int],
    passes: Iterable[Test],
    fails: Iterable[Test],
    testLocMap: Map[Test, Iterable[Int]],
  ): Map[Test, IndexedSeq[Double]] =
    val passNum = passes.size

    // initial stat map
    val initStats = ids.map(_ => LocStat(0, 0, passNum, 1))

    // update stat of each locations by iterating all successful tests
    val passStats = passes.foldLeft(initStats) {
      case (stats, pass) =>
        val touchedLocs = testLocMap(pass)
        touchedLocs.foldLeft(stats)(updateStat(true))
    }

    // calculate score for each failed tests
    fails
      .map(fail => {
        val touchedLocs = testLocMap(fail)
        val stats =
          touchedLocs.foldLeft(passStats)(updateStat(false))

        (fail, stats.map(_.score))
      })
      .toMap

  /** quadruple of location stat */
  case class LocStat(ep: Int, ef: Int, np: Int, nf: Int) {
    def touch(isPass: Boolean): LocStat = {
      if (isPass)
        LocStat(ep + 1, ef, np - 1, nf)
      else
        LocStat(ep, ef + 1, np, nf - 1)
    }
    lazy val score = ef - ep / (ep + np + 1.0)
  }

  private def updateStat(pass: Boolean) =
    (stats: IndexedSeq[LocStat], idx: Int) =>
      stats.updated(idx, stats(idx).touch(pass))

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "debug",
      BoolOption(c => c.debug = true),
      "turn on debug mode",
    ),
    (
      "topN",
      NumOption((c, k) => c.topN = Some(k)),
      "set the number for top rankings to keep",
    ),
  )
  case class Config(
    var debug: Boolean = false,
    var topN: Option[Int] = Some(100),
  )
}
