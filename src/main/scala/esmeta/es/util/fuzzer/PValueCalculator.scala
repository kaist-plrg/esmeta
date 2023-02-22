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

object PValueCalculator {
  val ALL = 2
  val PARTIAL = 1
  val NO_DEBUG = 0

  def getPValues(
    logInterval: Option[Int] = Some(600), // default is 10 minutes.
    debug: Int = NO_DEBUG, // 2: all, 1: partial, 0: no
    stdOut: Boolean = false,
    timeLimit: Option[Int] = None, // time limitation for each evaluation
    trial: Option[Int] = None, // `None` denotes no bound
    duration: Option[Int] = None, // `None` denotes no bound
    kFs: Int = 0, // feature sensitivity bias
    cp: Boolean = false,
    preFuzzIter: Int = 1,
  ): Map[String, Double] = {
    val covPre = Fuzzer(
      logInterval = logInterval,
      debug = debug,
      timeLimit = timeLimit,
      trial = trial,
      duration = duration,
      kFs = kFs,
      cp = cp,
    )
    val featureInOuts = covPre.featureInOuts
    val pValueMap = featureInOuts.view.mapValues(StatUtils.chiSqIdpTest).toMap
    pValueMap
  }
}
