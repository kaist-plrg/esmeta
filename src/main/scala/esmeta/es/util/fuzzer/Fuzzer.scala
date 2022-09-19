package esmeta.es.util.fuzzer

import esmeta.cfg.CFG
import esmeta.es.util.*

/** ECMAScript program fuzzer with ECMA-262 */
object Fuzzer:
  def apply(
    cfg: CFG,
    log: Boolean = false,
    timeLimit: Option[Int] = None,
  ): Set[String] = new Fuzzer(cfg, log, timeLimit).result

/** extensible helper of ECMAScript program fuzzer with ECMA-262 */
class Fuzzer(
  cfg: CFG,
  log: Boolean = false,
  timeLimit: Option[Int] = None,
) {

  /** generated ECMAScript programs */
  lazy val result: Set[String] = ???

  /** coverage */
  lazy val cov: Coverage = Coverage(cfg, timeLimit = timeLimit)
}
