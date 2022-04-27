package esmeta.editor

import esmeta.editor.sview.*
import esmeta.editor.util.CFGHelper
import esmeta.ir.util.UnitWalker
import esmeta.ir.Inst
import esmeta.ir.NormalInst
import esmeta.ir.IIf
import esmeta.ir.ILoop
import esmeta.ir.ISeq
import esmeta.editor.util.*
import esmeta.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import java.io.PrintWriter
import java.io.FileReader
import org.scalatest.ParallelTestExecution
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration.Duration

class AnalysisSoundnessTest extends ESMetaAsyncTest {
  val name: String = "analysisSoundnessTest"
  def category: String = "editor"

  // predefined data
  lazy val logDir = s"$LOG_DIR/editor_$dateStr"
  var pw: Option[PrintWriter] = None

  // registration
  def init: Unit =
    val cfgHelper = CFGHelper(EditorTest.cfg)
    val af = AnalysisFrontend(cfgHelper)
    val idMap = cfgHelper.cfg.fnameMap.map { case (s, f) => (f.id, s) }.toMap
    val viewList = BasicSyntacticView(cfgHelper).diffViewSet.toList
      .sortBy(_._1)
    val mcgs = EditorTest.cfg.funcs
      .map((f) => f.name -> SyntacticCallGraph(cfgHelper, f.irFunc))
      .toMap
    mkdir(logDir)
    mkdir(s"${logDir}/detail")
    pw = Some(getPrintWriter(s"${logDir}/${name}.log"))
    val time = System.currentTimeMillis
    test("topTest") {
      Await.result(
        Future.sequence(viewList.map {
          case (name, v) =>
            check(
              s"$name (${v.name}): ${v.toString(true, false, Some(EditorTest.cfg.grammar))}",
            ) {
              val time = System.currentTimeMillis

              val analysisCG = af.cg(v)
              val actualSet = readFile(s"./soundness/$name")
                .split("\n")
                .filter((s) => s.length != 0)
                .map((s) => idMap(s.toInt))
                .toSet

              pw.synchronized {
                pw.foreach((pw) => {
                  pw.println(s"$name: ${v
                    .toString(true, false, Some(EditorTest.cfg.grammar))}")
                  pw.println(s"staticCG missed func: ${actualSet
                    .filter((s) => (!analysisCG.funcs.contains(s)))
                    .map((s) => (s, cfgHelper.cfg.fnameMap(s).id))
                    .toList
                    .sorted
                    .mkString(", ")}")
                })
              }
              val thispw = getPrintWriter(s"${logDir}/detail/${name}.log")
              analysisCG.funcs.toList.sorted.foreach(thispw.println(_))
              thispw.close
              assert(
                actualSet.forall((s) =>
                  analysisCG.funcs
                    .contains(s),
                ),
              )
              println((System.currentTimeMillis - time) / 1000)

            },
        }),
        Duration.Inf,
      )
    }

  override def afterAll(): Unit = {
    pw.foreach(_.close)
    super.afterAll()
  }
  init

}
