package esmeta.phase

import esmeta.*
import esmeta.cfg.Func
import esmeta.editor.Minifier

/** `view-minify` phase */
case object ViewMinify extends Phase[Func, Func] {
  val name = "view-minify"
  val help = "minify the given function"
  def apply(
    func: Func,
    globalConfig: GlobalConfig,
    config: Config,
  ): Func =
    Minifier(func)()

  def defaultConfig: Config = Config()

  val options: List[PhaseOption[Config]] = List()
  case class Config()
}
