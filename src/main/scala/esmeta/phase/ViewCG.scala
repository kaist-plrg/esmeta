package esmeta.phase

import esmeta.*
import esmeta.interp.*
import esmeta.ir.*
import esmeta.util.SystemUtils.*
import esmeta.util.*
import esmeta.editor.sview.SyntacticView
import esmeta.cfg.CFG
import esmeta.editor.AnalysisFrontend
import esmeta.editor.util.CFGHelper
import java.io.PrintWriter
import esmeta.editor.SyntacticCallGraph
import esmeta.editor.SyntacticTransitiveClosedCallGraph

/** `view-cg` phase */
case object ViewCG extends Phase[(CFG, SyntacticView), Unit] {
  val name = "view-cg"
  val help = "Draw a call graph with a Syntactic View."
  def apply(
    cfgWithview: (CFG, SyntacticView),
    globalConfig: GlobalConfig,
    config: Config,
  ): Unit =
    val (cfg, view) = cfgWithview
    val cfgHelper = CFGHelper(cfg)
    // BasicSyntacticView(cfgHelper).viewSet.foreach{ case (s, v) => println(s"$s -> ${v.map((x) => x.name + " " + x.toString(false, false, Some(cfg.grammar))).mkString(", ")}")}
    val nview = view.getNormal(cfgHelper).get.folded.invalidateFold

    val cg =
      if (config.useSyntactic)
        val mcgs = cfg.funcs
          .map((f) => f.name -> SyntacticCallGraph(cfgHelper, f.irFunc))
          .toMap
        SyntacticTransitiveClosedCallGraph(
          mcgs,
          cfgHelper.getSDOView(nview, "Evaluation").get._2.name,
        )
      else
        val af =
          AnalysisFrontend(cfgHelper, verbose = true, repl = config.useRepl)
        af.cg(nview)

    val writer = PrintWriter("peval.dot")
    writer.println(cg.toDot)
    writer.close
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "syntactic",
      BoolOption(c => c.useSyntactic = true),
      "use cg with syntactic closure.",
    ),
    (
      "repl",
      BoolOption(c => c.useRepl = true),
      "use repl when finish analysis",
    ),
  )
  case class Config(
    var useSyntactic: Boolean = false,
    var useRepl: Boolean = false,
  )
}
