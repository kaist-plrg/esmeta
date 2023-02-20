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

  def evaluate(
    baseDir: String,
    targets: Iterable[Target],
  ): (Double, Double, Int) =
    println("evaluation start...")
    val numMinimals = getCoverage(baseDir).minimalScripts.size
    var expGroup: Map[Int, Int] = Map()
    val fileList = listFiles(s"$BASE_DIR/reported-bugs")
    println(s"found ${fileList.size} js bug files")
    println(s"# of minimals: $numMinimals")
    var idx = 0
    var cleanHit = 0
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
      val (_, _, covered, blockingSet) = cov.runAndCheckBlockings(script)
      val minBugCodeSize =
        if covered || blockingSet.isEmpty then {
          cleanHit += 1; println(s"index $idx clean hit! total $cleanHit .");
          1000
        } else
          blockingSet.toList
            .sortBy(-_.code.length)
            .foldLeft(0) {
              case (codeSize, script) =>
                if codeSize >= script.code.length then codeSize
                else if (
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
                then 0
                else script.code.length
            }

      expGroup += idx -> minBugCodeSize
      idx += 1
    }
    println("controlGroup start ######################")
    println(expGroup)
    println("#########################################")
    println(s"# of clean hits: $cleanHit")
    println(
      s"dirty hit ratio: ${(1 - cleanHit / fileList.size.toDouble) * 100}",
    )
    println(
      s"dirty hit average: ${expGroup.values.filter(_ != 1000).sum / expGroup.values.count(_ != 1000).toDouble}",
    )

    (0, 0, numMinimals)

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
