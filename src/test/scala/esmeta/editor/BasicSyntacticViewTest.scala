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

  var maV: Double = 0
  var maE: Double = 0
  var taV: Double = 0
  var taE: Double = 0
  var s = 0
  // registration
  def init: Unit =
    val cfgHelper = CFGHelper(EditorTest.cfg)
    val af = AnalysisFrontend(cfgHelper)
    val viewList = BasicSyntacticView(cfgHelper).diffViewSet.toList
      // .map((x) => (x._1, x._2.refined(cfgHelper)))
      // .collect { case (i, s: Syntactic) => (i, s) }
      // .filter(cfgHelper.getSDOView(_, "Evaluation").isDefined)
      .sortBy(_._1)
    val mcgs = EditorTest.cfg.funcs
      .map((f) => f.name -> SyntacticCallGraph(cfgHelper, f.irFunc))
      .toMap
    val mergeCG = SyntacticMergedCallGraph(mcgs, cfgHelper)
    mkdir(logDir)
    mkdir(s"${logDir}/detail")
    pw = Some(getPrintWriter(s"${logDir}/${name}.log"))

    viewList.foreach {
      case (name, v) =>
        check(
          s"$name (${v.name}): ${v.toString(true, false, Some(EditorTest.cfg.grammar))}",
        ) {
          // Parser(EditorTest.cfg.grammar)(v.name, v.args)
          //  .from(v.toString(grammar = Some(EditorTest.cfg.grammar)))
          val transitiveCG = SyntacticTransitiveClosedCallGraph(
            mcgs,
            cfgHelper.getSDOView(v, "Evaluation").get._2.name,
          )
          val analysisCG = af.cg(v)
          pw.foreach((pw) => {
            pw.println(s"$name: ${v
              .toString(true, false, Some(EditorTest.cfg.grammar))}")

            val (mV, mE) = (
              mergeCG.funcs.size,
              mergeCG.func_targets.toSet.flatMap {
                case (i, j) => j.map((k) => (i -> k))
              }.size,
            )
            val (tV, tE) = (
              transitiveCG.funcs.size,
              transitiveCG.func_targets.toSet.flatMap {
                case (i, j) => j.map((k) => (i -> k))
              }.size,
            )
            val (aV, aE) = (
              analysisCG.funcs.size,
              analysisCG.func_targets.toSet.flatMap {
                case (i, j) => j.map((k) => (i -> k))
              }.size,
            )
            val maVi = (aV.toDouble / mV.toDouble)
            val taVi = (aV.toDouble / tV.toDouble)
            val maEi = (if (mE == 0) 1 else (aE.toDouble / mE.toDouble))
            val taEi = (if (tE == 0) 1 else (aE.toDouble / tE.toDouble))
            maV += maVi
            taV += taVi

            maE += maEi
            taE += taEi
            s += 1
            pw.println(
              s"    ${mV}/${mE}  -> ${tV}/${tE} -> ${aV}/${aE} (%${f"${maVi * 100}%2.2f"}/%${f"${maEi * 100}%2.2f"} -> %${f"${taVi * 100}%2.2f"}/%${f"${taEi * 100}%2.2f"})",
            )
            val thispw = getPrintWriter(s"${logDir}/detail/${name}.log")
            analysisCG.funcs.toList.sorted.foreach(thispw.println(_))
            thispw.close
            // println(
            //  s"    ${mV}/${mE}  -> ${tV}/${tE} -> ${aV}/${aE} (%${f"${maVi * 100}%2.2f"}/%${f"${maEi * 100}%2.2f"} -> %${f"${taVi * 100}%2.2f"}/%${f"${taEi * 100}%2.2f"})",
            // )
          })
        },
    }

  override def afterAll(): Unit = {
    pw.map((pw) => {
      pw.println("Total:")
      pw.println(
        s"    %${f"${maV / s * 100}%2.2f"}/%${f"${maE / s * 100}%2.2f"} -> %${f"${taV / s * 100}%2.2f"}/%${f"${taE / s * 100}%2.2f"}",
      )
      // println(
      //  s"    %${f"${maV / s * 100}%2.2f"}/%${f"${maE / s * 100}%2.2f"} -> %${f"${taV / s * 100}%2.2f"}/%${f"${taE / s * 100}%2.2f"}",
      // )

      pw.close
    })
    super.afterAll()
  }
  init

}
