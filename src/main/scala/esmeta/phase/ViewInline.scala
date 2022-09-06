package esmeta.phase

import esmeta.*
import esmeta.cfg.{CFG, Node, Func}
import esmeta.editor.Inliner
import esmeta.editor.sview.SyntacticView

/** `view-inline` phase */
case object ViewInline extends Phase[(CFG, SyntacticView), Func] {
  val name = "view-inline"
  val help = "Transform Syntactic View into single, inlined algorithm."
  def apply(
    cfgWithView: (CFG, SyntacticView),
    globalConfig: GlobalConfig,
    config: Config,
  ): Func =
    val (cfg, view) = cfgWithView
    Inliner(cfg, view)()

  def defaultConfig: Config = Config()

  val options: List[PhaseOption[Config]] = List()
  case class Config()
}
