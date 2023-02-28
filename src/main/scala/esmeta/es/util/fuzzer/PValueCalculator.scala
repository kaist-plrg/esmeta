package esmeta.es.util.fuzzer

import esmeta.cfg.{CFG, Node}
import esmeta.es.util.Coverage.*
import esmeta.es.util.fuzzer.*
import esmeta.es.util.{Coverage, withCFG}
import esmeta.state.Feature
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import esmeta.{error as _, *}

class PValueCalculator(
  logInterval: Option[Int] = Some(600), // default is 10 minutes.
  debug: Int = 1, // 2: all, 1: partial, 0: no
  stdOut: Boolean = false,
  timeLimit: Option[Int] = None, // time limitation for each evaluation
  trial: Option[Int] = None, // `None` denotes no bound
  duration: Option[Int] = None, // `None` denotes no bound
  kFs: Int = 0, // feature sensitivity bias
  cp: Boolean = false,
  preFuzzIter: Int = 1,
) {
  lazy val covPre: Coverage = Fuzzer(
    logInterval = logInterval,
    debug = debug,
    timeLimit = timeLimit,
    trial = trial,
    duration = duration,
    kFs = kFs,
    cp = cp,
    isPreFuzz = true,
  )

  def indepPValues: Map[String, Double] = {
    val featureInOuts = covPre.featureInOuts
    val pValueMap = featureInOuts.view.mapValues(StatUtils.chiSqIdpTest).toMap
    pValueMap
  }

  def comboPValues: Map[(String, String), Double] = {
    val featureCombos = covPre.featureCombos
    val totalCombos = covPre.totalCombos
    val pValueMap =
      featureCombos.view.mapValues(StatUtils.leftBinTest(totalCombos, 0.5, _))
    pValueMap.toMap
  }
}
