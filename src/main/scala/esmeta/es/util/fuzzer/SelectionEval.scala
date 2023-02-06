package esmeta.es.util.fuzzer

import esmeta.BASE_DIR
import esmeta.es.Script
import esmeta.es.util.injector.Injector
import esmeta.es.util.{Coverage, USE_STRICT}
import esmeta.js.Target
import esmeta.phase.ConformTest
import esmeta.util.SystemUtils.{listFiles, readFile}

object SelectionEval {
  lazy val bugDB: List[String] = ???

  def getCoverage(baseDir: String): Coverage = Coverage.fromLog(baseDir)

  def checkBug(bugScript: Script): Boolean = ???

  def evaluate(baseDir: String, targets: Iterable[Target]): (Int, Int) =
    println("evaluation start...")
    val numMinimals = getCoverage(baseDir).minimalScripts.size
    var controlGroup: Map[Int, Int] = Map()
    val fileList = listFiles(s"$BASE_DIR/reported-bugs")
    println(s"found ${fileList.size} js bug files")
    var idx = 0
    for {
      bugCode <- fileList
      name = bugCode.getName
      code = USE_STRICT + readFile(bugCode.getPath).trim()
      script = Script(code, name)
    } {
      if (idx % 10 == 0) {
        println(s"index: $idx")
      }
      val cov = getCoverage(baseDir)
      val (_, _, _, blockingSet) = cov.runAndCheckBlockings(script)
      val minBugCodeSize = blockingSet
        .filter(script =>
          !targets.foldLeft(false) {
            case (isTargetBug, target) =>
              if isTargetBug then true
              else
                ConformTest
                  .doConformTest(
                    target,
                    target.isTrans,
                    Script(
                      Injector(script.code, true, false).toString,
                      script.name,
                    ),
                    true,
                  )
                  .isDefined
          },
        )
        .map(_.code.length)
        .max

      controlGroup += idx -> minBugCodeSize
      idx += 1
    }
    println("controlGroup start ######################")
    println(controlGroup)
    println("#########################################")
    (0, numMinimals) // TODO: Replace Zero

  private val engineTargets: List[Target] = List(
    Target("d8", false),
    Target("node", false),
    Target("js", false),
    Target("sm", false),
    Target("jsc", false),
  )

  private val transTargets: List[Target] = List(
    Target("babel", true),
    Target("swc", true),
    Target("terser", true),
    Target("obfuscator", true),
  )
}
