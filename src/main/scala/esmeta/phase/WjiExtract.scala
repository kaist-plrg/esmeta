package esmeta.phase

import esmeta.*
import esmeta.util.*
import esmeta.wji.extractor.Extractor
import esmeta.wji.spec.Spec
import esmeta.wji.lang.Algorithm
import esmeta.wji.lang.printer.InstrPrinter

/** `wji-extract` phase
  *
  * Loads WJI algorithms from all specs (jsApi + the filtered webidl subset).
  * Optionally narrows them with a substring `filter` and prints the rendered
  * metalanguage ASTs (replaces the old `printAlgorithms` entry point).
  */
case object WjiExtract extends Phase[Unit, Spec] {
  val name = "wji-extract"
  val help = "extracts WJI algorithms from the WebAssembly JS API specs."

  def apply(
    unit: Unit,
    cmdConfig: CommandConfig,
    config: Config,
  ): Spec =
    val spec = Extractor()
    // must happen before the interpreter runs (see RecordTy's doc) — done
    // here since every wji-eval/wji-interp run passes through this phase
    spec.registerDefinitionTypes()
    val selected = select(spec.algorithms, config.filter)
    if (config.log)
      if (selected.isEmpty) println(s"no algorithm matches '${config.filter}'")
      else
        println(s"${selected.size} algorithm(s)")
        for (algo <- selected) println(InstrPrinter.render(algo))
    spec.copy(algorithms = selected)

  /** keep algorithms whose `name` or `id` contains `filter` (case-insensitive)
    */
  private def select(algos: List[Algorithm], filter: String): List[Algorithm] =
    if (filter.isEmpty) algos
    else
      val needle = filter.toLowerCase
      algos.filter(a => (a.name ++ a.id).exists(_.toLowerCase.contains(needle)))

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "filter",
      StrOption((c, s) => c.filter = s),
      "only keep algorithms whose name or id contains this substring.",
    ),
    (
      "log",
      BoolOption(_.log = _),
      "print the rendered algorithm ASTs.",
    ),
  )
  case class Config(
    var filter: String = "",
    var log: Boolean = false,
  )
}
