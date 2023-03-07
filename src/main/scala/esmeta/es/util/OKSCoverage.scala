package esmeta.es.util

class OKSCoverage(
  timeLimit: Option[Int] = None,
  kFs: Int = 0,
  cp: Boolean = false,
  isPreFuzz: Boolean = false,
  nodeViewKMap: Map[String, Int] = Map[String, Int]().withDefaultValue(0),
  condViewKMap: Map[String, Int] = Map[String, Int]().withDefaultValue(0),
  pValueMapOpt: Option[Map[String, Double]] = None,
) extends Coverage(
    timeLimit,
    kFs,
    cp,
    isPreFuzz,
    nodeViewKMap,
    condViewKMap,
    pValueMapOpt,
  ) {
  // someday refactoring
}
