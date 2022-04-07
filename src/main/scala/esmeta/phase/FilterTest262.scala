package esmeta.phase

import esmeta.*
import esmeta.test262.*
import esmeta.test262.util.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import esmeta.cfg.CFG
import esmeta.editor.util.*
import scala.collection.mutable.ListBuffer

/** `filter-test262` phase */
case object FilterTest262 extends Phase[CFG, List[String]] {
  val name = "filter-test262"
  val help = "filters Test262 tests for using syntactic view."
  def apply(
    cfg: CFG,
    globalConfig: GlobalConfig,
    config: Config,
  ): List[String] = {
    val svParser = Parser(cfg.grammar)("Script")
    val filename = getFirstFilename(globalConfig, this.name)
    val sview = svParser.fromFile(filename)

    // test262 configuration
    val test262 = Test262(cfg.spec)
    val test262Config =
      config.test262List.fold(test262.config)(TestFilter.fromFile)

    // progress bar
    val progress =
      ProgressBar("filter test262", test262Config.normal)

    // filtered tests
    var parseTime = 0L
    var containsTime = 0L
    val filtered: ListBuffer[String] = ListBuffer()
    for (c <- progress) {
      val NormalConfig(name, _) = c
      val (ptime, ast) = time(
        cfg.jsParser("Script").fromFile(s"$TEST262_TEST_DIR/$name"),
      )
      val (ctime, contained) = time(ast contains sview)
      if (contained) filtered += name

      parseTime += ptime
      containsTime += ctime
    }

    println(parseTime)
    println(containsTime)
    println(filtered.size)

    filtered.toList
  }
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "test262-list",
      StrOption((c, s) => c.test262List = Some(s)),
      "use given test262 tests list.",
    ),
  )
  case class Config(
    var test262List: Option[String] = None,
  )
}
