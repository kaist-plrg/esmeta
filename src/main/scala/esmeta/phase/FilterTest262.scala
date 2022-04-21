package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import esmeta.editor.util.{Filter, Parser}
import esmeta.editor.sview.{SyntacticView, Syntactic}

/** `filter-test262` phase */
case object FilterTest262 extends Phase[CFG, Unit] {
  val name = "filter-test262"
  val help = "filters Test262 tests using syntactic view."
  def apply(
    cfg: CFG,
    globalConfig: GlobalConfig,
    config: Config,
  ): Unit = {
    // get syntactic view
    val svParser = Parser(cfg.grammar)("Script") // TODO goal symbol
    val filename = getFirstFilename(globalConfig, this.name)
    val sview = svParser.fromFile(filename)

    // find actual root of syntactic view
    // TODO remove
    def getRoot(sv: SyntacticView): SyntacticView = sv match
      case Syntactic(_, _, _, List(Some(child))) => getRoot(child)
      case _                                     => sv
    val sviewRoot = getRoot(sview)

    // // filter using syntactic view
    // config.test262Data match
    //   case Some(path) =>
    //     // TODO use algo id
    //     val (testNames, algoSet) = Filter(cfg, path, sviewRoot)
    //     for { name <- testNames } println(name)
    //     println(algoSet)
    //     println(testNames.size)
    //   case None => ???
  }
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "test262-data",
      StrOption((c, s) => c.test262Data = Some(s)),
      "use given test262 tests data.",
    ),
    (
      "aid",
      NumOption((c, n) => c.aid = Some(n)),
      "find tests with syntactic view and given algo id",
    ),
  )
  case class Config(
    var test262Data: Option[String] = None,
    var useRegex: Boolean = false,
    var aid: Option[Int] = None,
  )
}
