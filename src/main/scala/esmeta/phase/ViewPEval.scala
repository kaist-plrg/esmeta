package esmeta.phase

import esmeta.*
import esmeta.interp.*
import esmeta.ir.*
import esmeta.util.SystemUtils.*
import esmeta.editor.sview.SyntacticView
import esmeta.cfg.CFG
import esmeta.editor.PartialEval
import esmeta.editor.util.CFGHelper
import java.io.PrintWriter

/** `view-peval` phase */
case object ViewPEval extends Phase[(CFG, SyntacticView), Unit] {
  val name = "view-peval"
  val help = "partial evaluates a Syntactic View."
  def apply(
    cfgWithview: (CFG, SyntacticView),
    globalConfig: GlobalConfig,
    config: Config,
  ): Unit =
    val (cfg, view) = cfgWithview
    val cfgHelper = CFGHelper(cfg)
    // BasicSyntacticView(cfgHelper).viewSet.foreach{ case (s, v) => println(s"$s -> ${v.map((x) => x.name + " " + x.toString(false, false, Some(cfg.grammar))).mkString(", ")}")}
    val peval = PartialEval(cfgHelper, verbose = true)
    val writer = PrintWriter("peval.dot")
    val nview = view.getNormal(cfgHelper).get.folded.invalidateFold
    writer.println(peval.cg(nview).toDot)
    writer.close
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List()
  case class Config()
}
