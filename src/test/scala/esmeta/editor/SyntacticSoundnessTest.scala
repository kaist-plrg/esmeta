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

class SyntacticSoundnessTest extends EditorTest {
  val name: String = "syntacticSoundnessTest"
  var pw: Option[PrintWriter] = None

  // registration
  def init: Unit =
    val cfgHelper = CFGHelper(EditorTest.cfg)
    val idMap = cfgHelper.cfg.fnameMap.map { case (s, f) => (f.id, s) }.toMap
    val viewList = BasicSyntacticView(cfgHelper).viewSet.toList
      // .map((x) => (x._1, x._2.refined(cfgHelper)))
      // .collect { case (i, s: Syntactic) => (i, s) }
      // .filter(cfgHelper.getSDOView(_, "Evaluation").isDefined)
      .sortBy(_._1)
    val mcgs = EditorTest.cfg.funcs
      .map((f) => f.name -> SyntacticCallGraph(cfgHelper, f.irFunc))
      .toMap
    mkdir(logDir)
    pw = Some(getPrintWriter(s"${logDir}/${name}.log"))

    viewList.foreach {
      case (name, v) =>
        check(
          s"$name (${v.name}): ${v.toString(true, false, Some(EditorTest.cfg.grammar))}",
        ) {
          val transitiveCG = SyntacticTransitiveClosedCallGraph(
            mcgs,
            cfgHelper.getSDOView(v, "Evaluation").get._2.name,
          )
          val actualSet = readFile(s"./basic_result/$name")
            .split("\n")
            .filter((s) => s.length != 0)
            .map((s) => idMap(s.toInt))
            .toSet
          pw.foreach((pw) => {
            pw.println(s"$name: ${v
              .toString(true, false, Some(EditorTest.cfg.grammar))}")
            pw.println(s"transitiveCG missed func: ${actualSet
              .filter((s) =>
                (s != "GetValue") &&
                (!transitiveCG.funcs.contains(s)),
              )
              .mkString(", ")}")
          })
          assert(
            actualSet.forall((s) =>
              (s == "GetValue") || transitiveCG.funcs.contains(s),
            ),
          )
        },
    }

  override def afterAll(): Unit = {
    pw.map(_.close)
    super.afterAll()
  }
  init

}
