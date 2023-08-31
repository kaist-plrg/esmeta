package esmeta.es.util.fuzzer

import esmeta.{BASE_DIR, PREFUZZEVAL_LOG_DIR}
import esmeta.es.Script
import esmeta.es.util.injector.Injector
import esmeta.es.util.{Coverage, USE_STRICT}
import esmeta.js.Target
import esmeta.phase.ConformTest
import esmeta.util.BaseUtils.dateStr
import esmeta.util.SystemUtils.{
  createSymlink,
  getPrintWriter,
  listFiles,
  mkdir,
  readFile,
}
import esmeta.cfg.CFG
import esmeta.ir.FuncKind.*

object SelectionEval {
  lazy val bugDB: List[String] = ???
  lazy val logDir: String = s"$PREFUZZEVAL_LOG_DIR/eval-$dateStr"
  lazy val symlink: String = s"$PREFUZZEVAL_LOG_DIR/recent"

  def getCoverage(baseDir: String): Coverage = Coverage.fromLog(baseDir)

  def checkBug(bugScript: Script): Boolean = ???

  def countAllFeatures: Int = 2104
//    val cfg = CFG.defaultCFG
//    var count = 0
//    for {
//      func <- cfg.funcs
//      kind = func.irFunc.kind
//    } {
//      kind match
//        case SynDirOp => count += 1
//        case Builtin  => count += 1
//        case _        => ()
//    }
//    count

  def evaluate(
    baseDir: String,
    targets: Iterable[Target],
  ): (Double, Double, Int) =
    println("evaluation start...")
    mkdir(logDir, remove = true)
    createSymlink(symlink, logDir, overwrite = true)
    val dw = getPrintWriter(s"$logDir/detail.txt")
    val sw = getPrintWriter(s"$logDir/summary.txt")

    val numMinimals = getCoverage(baseDir).minimalScripts.size
    var expGroup: Map[Int, Int] = Map()
    val fileList = listFiles(s"$BASE_DIR/reported-bugs")
    println(s"found ${fileList.size} js bug files")
    println(s"# of minimals: $numMinimals")
    var idx = 0
    var cleanHit = 0
    val cov = getCoverage(baseDir)
    for {
      bugCode <- fileList
      name = bugCode.getName
      code = USE_STRICT + readFile(bugCode.getPath).trim()
      script = Script(code, name)
    } {
//      if (idx % 10 == 0) {
//        println(s"index: $idx")
//      }
      val (_, _, covered, blockingSet) = cov.runAndCheckBlockings(script)
      val minBugCodeSize =
        if covered || blockingSet.isEmpty then {
          cleanHit += 1;
          println(s"index $idx clean hit! total $cleanHit .");
          dw.println(s"$idx : $name : Unconditionally survive")
          -1
        } else
          val blockingLength = blockingSet.toList
            .map(_.code.length)
            .sortBy(-_)
            .headOption
            .getOrElse(0)
          dw.println(s"$idx : $name : $blockingLength")
          println(s"index $idx dirty hit! length $blockingLength .");
          blockingLength

      expGroup += idx -> minBugCodeSize
      idx += 1
    }
    dw.close()
    println("controlGroup start ######################")
    println(expGroup)
    println("#########################################")

    sw.println(s"eval $baseDir")
    sw.println(s"# of minimals: $numMinimals")
    sw.println(s"# of clean hits: $cleanHit / ${fileList.size}")
    sw.println(
      s"dirty hit ratio: ${(1 - cleanHit / fileList.size.toDouble) * 100}",
    )
    sw.println(
      s"dirty hit average: ${expGroup.values.filter(_ != -1).sum / expGroup.values.count(_ != -1).toDouble}",
    )
    sw.close()

    println(s"# of clean hits: $cleanHit")
    println(
      s"dirty hit ratio: ${(1 - cleanHit / fileList.size.toDouble) * 100}",
    )
    println(
      s"dirty hit average: ${expGroup.values.filter(_ != -1).sum / expGroup.values.count(_ != -1).toDouble}",
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
