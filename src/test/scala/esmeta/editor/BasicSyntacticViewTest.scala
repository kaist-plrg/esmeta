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

class InstCounter extends UnitWalker {
  var instCount = 0
  var branchCount = 0
  override def walk(i: Inst) = i match
    case IIf(_, trueInst, falseInst) => {
      instCount += 1; branchCount += 1; walk(trueInst); walk(falseInst)
    }
    case ILoop(_, _, body) => { instCount += 1; branchCount += 1; walk(body) }
    case ISeq(insts)       => insts.foreach(walk)
    case _                 => instCount += 1

  def summary: (Int, Int) = (instCount, branchCount)
}

class BasicSyntacticViewTest extends EditorTest {
  val name: String = "basicSyntacticViewTest"
  var pw: Option[PrintWriter] = None

  // registration
  def init: Unit =
    val peval = PartialEval(CFGHelper(EditorTest.cfg))
    val viewSet = BasicSyntacticView(CFGHelper(EditorTest.cfg)).viewSet2
    mkdir(logDir)
    pw = Some(getPrintWriter(s"${logDir}/${name}.log"))

    viewSet.foreach((v) =>
      check(
        s"${v.name}${v.rhsIdx}: ${v.toString(true, false, Some(EditorTest.cfg.grammar))}",
      ) {
        // println(v.toString(true, false,Some(EditorTest.cfg.grammar)))
        val flist = peval(v)
        val summaries = flist.map((f) => {
          val origF = EditorTest.cfg.fnameMap(f.name).irFunc
          val o1 = InstCounter()
          val o2 = InstCounter()
          o1.walk(f); o2.walk(origF)
          (f.name, o2.summary, o1.summary)
        })
        val (origSummary, redSummary) = summaries.foldLeft(((0, 0), (0, 0))) {
          case (((i, j), (k, l)), (_, (a, b), (c, d))) =>
            ((i + a, j + b), (k + c, l + d))
        }
        pw.foreach((pw) =>
          pw.println(s"${v.name}${v.rhsIdx}: ${v
            .toString(true, false, Some(EditorTest.cfg.grammar))}")
          pw.println(
            s"    ${origSummary._1}/${origSummary._2}/${EditorTest.cfg.funcs.length} -> ${redSummary._1}/${redSummary._2}/${flist.length}",
          ),
        )
        // mv = math.min(mv, origSummary._1 - redSummary._1)
        // if (Mv < origSummary._1 - redSummary._1)
        //  println(v.toString(true, false, Some(EditorTest.cfg.grammar)))
        // Mv = math.max(Mv, origSummary._1 - redSummary._1)
        // z += (origSummary._1 - redSummary._1)
        // s += 1
        // summaries.foreach {
        // case (name, (a, b), (c, d)) => {
        //   pw.foreach(_.println(s"  - ${name}"))
        //    pw.foreach(_.println(s"    $a/$b -> $c/$d"))
        //  }
        // }
        // println((mv, Mv, z / s))
      },
    )

  override def afterAll(): Unit = {
    pw.map(_.close)
    super.afterAll()
  }
  init

}
