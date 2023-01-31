package esmeta.phase

import esmeta.*
import esmeta.error.*
import esmeta.es.Script
import esmeta.es.util.injector.*
import esmeta.js.*
import esmeta.js.Bug.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.util.BaseUtils.*
import scala.collection.parallel.CollectionConverters._

/** `conform-test` phase */
case object ConformTest
  extends Phase[
    (
      Map[Target, Iterable[Script]],
      Map[Target, Iterable[Script]],
      Iterable[Script],
    ),
    Map[Target, Iterable[String]],
  ] {
  val name = "conform-test"
  val help = "Perform conformance test for an ECMAScript Engine or a transpiler"

  private var _config: Option[Config] = None
  private type Input = Map[Target, Iterable[Script]]
  private type Result = Map[Target, Iterable[String]]

  def apply(
    input: (Input, Input, Iterable[Script]),
    cmdConfig: CommandConfig,
    config: Config,
  ): Result =
    _config = Some(config)
    config.msgdir.foreach(cleanDir)
    cleanDir(s"$CONFORMTEST_LOG_DIR/msg")

    val (etestMap, ttestMap, originals) = input

    // generate name->original map
    originals.foreach(s => originalMap += (s.name -> s.code))

    // load bug db
    val bugDB = loadBugDB(etestMap.keys ++ ttestMap.keys)
    knownBugMap = bugDB._1
    todoBugMap = bugDB._2

    // do test for engines
    val engineResult = etestMap.map((e, tests) =>
      e -> {
        val fails = tests.par
          .flatMap(test => doConformTest(e, false, test))
          .toList

        bugStat += (e -> toBugStat(fails))

        fails.map(_._1)
      },
    )

    // do test for transpilers
    val transResult = ttestMap.map((t, tests) =>
      t -> {
        val fails = tests.par
          .flatMap(test => doConformTest(t, true, test))
          .toList

        bugStat += (t -> toBugStat(fails))

        fails.map(_._1)
      },
    )

    val result: Result = engineResult ++ transResult

    // dump result as JSON and tsv
    dumpJson(result.map(_.toString -> _), s"$CONFORMTEST_LOG_DIR/fails.json")
    dumpJson(
      bugStat.map(_.toString -> _),
      s"$CONFORMTEST_LOG_DIR/bug-stat.json",
    )
    dumpSummary(result, bugStat)

    // return result
    result

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------

  private def debug(a: Any): Unit =
    if (_config.exists(_.debug)) println(a)

  // get the original code by name
  private var originalMap: Map[String, String] = Map()

  // loaded bug DB
  private var knownBugMap: Map[Target, Map[String, Set[String]]] = Map()
  private var todoBugMap: Map[Target, Map[String, String]] = Map()

  // bug stat
  private var bugStat: Map[Target, Map[String, Int]] = Map()

  // do conform test, and returns Some((name, bug)) if fails.
  def doConformTest(
    target: Target,
    isTrans: Boolean,
    script: Script,
    isSimple: Boolean = false,
  ): Option[(String, String)] =
    val Script(code, name) = script
    debug(s"Testing $target: $name...")

    // Obtain expected result
    val exitTagRaw = exitTagPattern.findFirstIn(code).get
    val exitTag = ExitTag(exitTagRaw).get
    val isNormal = exitTag == NormalTag

    // Obtain concrete result
    val (concreteExitTag, stdout) =
      if (!isTrans) {
        JSEngine
          .runUsingBinary(target.cmd, code)
          .map((NormalTag, _))
          .recover(engineErrorResolver _)
          .get
      } else {
        JSEngine
          .run(code)
          .map((NormalTag, _))
          .recover(engineErrorResolver _)
          .get
      }

    val sameExitTag = exitTag.equivalent(concreteExitTag)
    val pass = sameExitTag && stdout.isEmpty

    // log information when test fails
    if pass then None
    else if isSimple then Some(name, "")
    else {
      val detail =
        if (!sameExitTag) then
          s"[Exit Tag Mismatch]$LINE_SEP > Expected $exitTagRaw but got $concreteExitTag"
        else s"[Assertion Fail]$LINE_SEP > $stdout"

      val original = originalMap.getOrElse(name, "")

      val tag = tagFinder(
        original.split(LINE_SEP)(1),
        Some(knownBugMap(target)),
        Some(todoBugMap(target)),
        Some(code),
        Some(detail),
      )

      val msg = s"""Test fail for `$target`: $name
                   |TAG: $tag
                   |$detail
                   |""".stripMargin

      debug(s"$original$LINE_SEP$msg")

      def dumpMsg(dir: String) = {
        val path = s"$dir/$name.msg"
        val orig = optional(readFile(path)).getOrElse(original)
        dumpFile(orig + LINE_SEP + msg, path)
      }
      _config.foreach(_.msgdir.foreach(dumpMsg))
      dumpMsg(s"$CONFORMTEST_LOG_DIR/msg/$target")

      if (_config.exists(_.saveBugs))
        val shortMsg = s"""TAG: ${tag.toString.replace("TODO", "NEW")}
                          |$detail""".stripMargin
        val data = s"$original$LINE_SEP/* $shortMsg */$LINE_SEP"
        tag match {
          case NewTag(_) => appendFile(data, s"$RESOURCE_DIR/bugs/$target/TODO")
          case TodoTag(_, path) => dumpFile(data, path)
          case _                =>
        }

      Some((name, tag._id))
    }

  private val exitTagPattern = "(?<=\\[EXIT\\] )[^\n\r]*".r
  private val errorPattern = "[\\w]*Error(?=: )".r

  private def engineErrorResolver(engineError: Throwable): (ExitTag, String) =
    engineError match {
      case e: TimeoutException     => (TimeoutTag, "")
      case e @ NoCommandError(cmd) => throw e
      case e =>
        val msg = e.getMessage
        val tag = errorPattern.findFirstIn(msg) match {
          case Some(name) =>
            ThrowErrorTag(name, msg.split(LINE_SEP).toList.headOption)
          case _ if msg.contains("TRANSPILE_FAILURE") => TranspileFailTag
          case _ => ThrowValueTag(esmeta.state.Str(msg))
        }
        (tag, "")
    }

  private def appendFile(data: Any, dir: String) = synchronized {
    // TODO: it seems that the synchronization here is not working properly
    // Maybe because system call is involved?
    val names = listFiles(dir).map(_.getName.dropRight(3).toInt).sorted
    val emptyIdx =
      names.zipWithIndex.find(p => p._1 != p._2).map(_._2).getOrElse(names.size)
    dumpFile(data, s"$dir/$emptyIdx.js")
  }

  private def toBugStat(fails: List[(String, String)]): Map[String, Int] =
    fails
      .map(_._2)
      .foldLeft(Map[String, Int]())((stat, tag) =>
        val origCount = stat.getOrElse(tag, 0)
        stat + (tag -> (origCount + 1)),
      )

  private def dumpSummary(
    result: Result,
    bugStat: Map[Target, Map[String, Int]],
  ) =
    val header = Vector("target", "bug-list", "fail-num")
    val body: List[Vector[String]] = result
      .map((target, fails) =>
        val bugs = bugStat(target).keys.map(_ + "|").mkString("")
        Vector(target.name, bugs, fails.size.toString),
      )
      .toList
    dumpRows(header :: body, s"$CONFORMTEST_LOG_DIR/test-summary.tsv")

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "debug",
      BoolOption(c => c.debug = true),
      "turn on debug mode",
    ),
    (
      "msgdir",
      StrOption((c, s) => c.msgdir = Some(s)),
      "directory to log msg",
    ),
    (
      "save-bugs",
      BoolOption(c => c.saveBugs = true),
      "save found bugs to database",
    ),
  )
  case class Config(
    var debug: Boolean = false,
    var msgdir: Option[String] = None,
    var saveBugs: Boolean = false,
  )
}
