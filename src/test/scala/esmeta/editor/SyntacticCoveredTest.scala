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

class SyntacticCoveredTest extends EditorTest {
  val name: String = "syntacticCoveredTest"
  var pw: Option[PrintWriter] = None

  // registration
  def init: Unit =
    val cfgHelper = CFGHelper(EditorTest.cfg)
    val af = AnalysisFrontend(cfgHelper)
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
          // Parser(EditorTest.cfg.grammar)(v.name, v.args)
          //  .from(v.toString(grammar = Some(EditorTest.cfg.grammar)))
          val transitiveCG = SyntacticTransitiveClosedCallGraph(
            mcgs,
            cfgHelper.getSDOView(v, "Evaluation").get._2.name,
          )
          val analysisCG = af.cg(v)
          val tedges = transitiveCG.func_targets.toList.flatMap {
            case (i, s) => s.map((i, _))
          }
          val aedges = analysisCG.func_targets.toList.flatMap {
            case (i, s) => s.map((i, _))
          }
          pw.foreach((pw) => {
            pw.println(s"$name: ${v
              .toString(true, false, Some(EditorTest.cfg.grammar))}")
            pw.println(s"transitiveCG missed func: ${analysisCG.funcs
              .filter((s) => (!transitiveCG.funcs.contains(s)))
              .mkString(", ")}")
            pw.println(
              s"transitiveCG missed edge: ${aedges.filter((v) => (!tedges.contains(v))).mkString(", ")}",
            )
          })
          assert(
            analysisCG.funcs.forall((s) =>
              transitiveCG.funcs.contains(s),
            ) && analysisCG.func_targets.forall {
              case (f, set) =>
                (transitiveCG.func_targets contains f) && set.forall((v) =>
                  transitiveCG.func_targets(f).contains(v),
                )
            },
          )
        },
    }

  override def afterAll(): Unit = {
    pw.map(_.close)
    super.afterAll()
  }
  init

}
