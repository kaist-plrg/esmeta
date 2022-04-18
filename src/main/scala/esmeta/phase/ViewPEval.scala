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
    val peval = PartialEval(CFGHelper(cfg), verbose = true)
    val writer = PrintWriter("peval.dot")
    writer.println(peval.cg(view).toDot)
    writer.close
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List()
  case class Config()
}
