package esmeta.fuzzer

import esmeta.cfg.CFG

/** ECMAScript program fuzzer with ECMA-262 */
object Fuzzer:
  def apply(
    cfg: CFG,
    log: Boolean = false,
  ): Set[String] = new Fuzzer(cfg, log).result

/** extensible helper of ECMAScript program fuzzer with ECMA-262 */
class Fuzzer(
  cfg: CFG,
  log: Boolean = false,
) {

  /** fuzzed ECMAScript programs */
  lazy val result: Set[String] = ???
}
