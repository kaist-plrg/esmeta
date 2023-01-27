package esmeta.es.util.fuzzer

import esmeta.es.util.Coverage

object SelectionEval {
  lazy val bugDB: List[String] = ???

  def getCoverage(baseDir: String): Coverage = Coverage.fromLog(baseDir)

  def checkBug(bugCode: String): Boolean = ???
}
