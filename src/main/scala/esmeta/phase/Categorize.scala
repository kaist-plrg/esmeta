package esmeta.phase

import esmeta.*
import esmeta.es.util.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.util.BaseUtils.*
import esmeta.js.Bug.*
import io.circe.*, io.circe.generic.semiauto.*
import io.circe.syntax._

/** `categorize` phase */
case object Categorize extends Phase[Unit, Map[String, Map[String, Int]]] {
  val name = "categorize"
  val help = "Categorize the bug"

  type Target = String
  type Tag = String
  type Test = String

  def apply(
    unit: Unit,
    cmdConfig: CommandConfig,
    config: Config,
  ): Map[Target, Map[Tag, Int]] =
    // handle inputs
    val scripts = cmdConfig.targets(0)
    val failsMapJson = cmdConfig.targets(1)
    val failsMap: Map[Target, Set[Test]] = readJson(failsMapJson)
    val testDir = cmdConfig.targets.lift(2)
    val msgDir = cmdConfig.targets.lift(3)

    val bugDB: Map[Target, Map[Tag, Set[String]]] = loadBugDB(failsMap.keys)._1

    val init: Map[Target, Map[Target, Int]] = Map()
    val result = failsMap.foldLeft(init) {
      case (cur, (target, fails)) =>
        cur + (target -> fails.foldLeft(Map[Target, Int]()) {
          case (count, test) =>
            val script = readFile(s"$scripts/$test").split(LINE_SEP)(1).trim
            val testOpt = testDir.map(d => readFile(s"$d/$target/$test"))
            val msgOpt = msgDir.map(d => readFile(s"$d/$target/$test.msg"))
            val tag = tagFinder(
              script,
              Some(bugDB(target)),
              None,
              testOpt,
              msgOpt,
            )._id
            if (!blackList.contains(tag))
              val c = count.getOrElse(tag, 0) + 1
              count + (tag -> c)
            else count
        })
    }

    dumpJson(result, s"$CATEGORIZE_LOG_DIR/result.json")
    dumpSummary(result)

    result

  private def dumpSummary(result: Map[Target, Map[Tag, Int]]) =
    val header = Vector("target", "bug-list", "fail-num")
    val body: List[Vector[String]] = result
      .map((target, bugCount) =>
        val bugs = bugCount.keys.map(_ + "|").mkString("")
        val failNum = bugCount.values.foldLeft(0)(_ + _)
        Vector(target, bugs, failNum.toString),
      )
      .toList
    dumpRows(header :: body, s"$CATEGORIZE_LOG_DIR/test-summary.tsv")

  private val blackList = List("OBF-V8", "ECM-01", "YET")

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List()
  case class Config()
}
