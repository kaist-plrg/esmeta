package esmeta.phase

import esmeta.*
import esmeta.cfg.*
import esmeta.es.util.*
import esmeta.es.util.Coverage.*
import esmeta.state.Feature
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
  type Location = FuncView | List[Feature]
  type Result = Map[Target, Map[Test, Seq[(Location, Double)]]]
  type MResult = MMap[Target, Map[Test, Seq[(Location, Double)]]]

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
    val featuresCoverageJson = s"$fuzzLogDir/feature-coverage.json"
    val touchedNodeViewJson = s"$fuzzLogDir/minimal-touch-nodeview.json"
    val touchedFeaturesJson = s"$fuzzLogDir/minimal-touch-features.json"

    // parse jsons
    val nodeViewInfos: Vector[NodeViewInfo] =
      readJson(nodeViewCoverageJson)
    val nodeViews = nodeViewInfos.map(_.nodeView)

    val featuresInfos: Vector[FeaturesInfo] =
      if (config.feature) readJson(featuresCoverageJson) else Vector()
    val features_s = featuresInfos.map(_.features)

    val test2NodeViewIds: Map[Test, Vector[Int]] =
      readJson(touchedNodeViewJson)
    val test2FeaturesIds: Map[Test, Vector[Int]] =
      if (config.feature) readJson(touchedFeaturesJson)
      else test2NodeViewIds.map((k, v) => k -> Vector())
    val tests = test2NodeViewIds.keys.toVector

    val target2Fails: Map[Target, Set[Test]] = readJson(failsMapJson)

    // iterate all targets and get localization result for each failed tests
    var result: MResult = MMap()
    target2Fails.foreach((target, fails) => {
      val passes = tests.filterNot(fails.contains(_))

      val nodeViewScores =
        localize(
          nodeViews,
          passes,
          fails,
          test2NodeViewIds.map(_ -> _.map(nodeViews)),
        )
      val funcViewScores =
        nodeViewScores.map(
          _ -> _.groupMapReduce(_._1.toFuncView)(_._2)(_ max _),
        )
      val featuresScores =
        localize(
          features_s,
          passes,
          fails,
          test2FeaturesIds.map(_ -> _.map(features_s)),
        )

      val mergedScores = funcViewScores.map((test, scores) => {
        test -> (scores ++ featuresScores(test)).toSeq
      })

      result(target) = mergedScores.map((test, scores) => {
        test -> scores.sortBy(_._2).reverse.slice(0, 10)
      })
    })

    // json encoder to dump result
    given LocEncoder: Encoder[Location] = new Encoder[Location] {
      final def apply(loc: Location): Json = Json.obj(loc match {
        case funcView: FuncView      => ("funcView", funcView.asJson)
        case features: List[Feature] => ("features", features.asJson)
      })
    }
    dumpJson(result, s"$LOCALIZE_LOG_DIR/localize.json")

    result.toMap
  }

  private def localize[L](
    locs: Iterable[L],
    passes: Iterable[Test],
    fails: Iterable[Test],
    testLocMap: Map[Test, Iterable[L]],
  ): Map[Test, Seq[(L, Double)]] =
    val passNum = passes.size
    val failNum = fails.size

    // initial stat map
    val initStatMap = locs.map(_ -> LocStat(0, 0, passNum, 1)).toMap

    // update stat of each locations by iterating all successful tests
    val passStatMap = passes.foldLeft(initStatMap) {
      case (statMap, pass) =>
        val touchedLocs = testLocMap(pass)
        touchedLocs.foldLeft(statMap)(updateStatMap(true))
    }

    // calculate score for each failed tests
    fails
      .map(fail => {
        val touchedLocs = testLocMap(fail)
        val statMap =
          touchedLocs.foldLeft(passStatMap)(updateStatMap(false))

        (fail, statMap.map(_ -> _.score).toSeq)
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

  private type StatMap[L] = Map[L, LocStat]
  private def updateStatMap[L](pass: Boolean) =
    (statMap: StatMap[L], loc: L) =>
      statMap.updatedWith(loc)(_.map(_.touch(pass)))

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "debug",
      BoolOption(c => c.debug = true),
      "turn on debug mode",
    ),
    (
      "feature",
      BoolOption(c => c.feature = true),
      "use 'feature' as localize candidate",
    ),
  )
  case class Config(
    var debug: Boolean = false,
    var feature: Boolean = false,
  )
}
