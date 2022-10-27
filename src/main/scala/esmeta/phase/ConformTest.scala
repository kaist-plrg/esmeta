package esmeta.phase

import esmeta.*
import esmeta.error.*
import esmeta.es.Script
import esmeta.es.util.injector.ConformTest.*
import esmeta.es.util.injector.*
import esmeta.js.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.util.BaseUtils.*

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

  private var _config: Config = null
  private var _onlySet: Option[Set[String]] = None
  private type Input = Map[Target, Iterable[Script]]
  private type Result = Map[Target, Iterable[String]]

  def apply(
    input: (Input, Input, Iterable[Script]),
    cmdConfig: CommandConfig,
    config: Config,
  ): Result =
    _config = config
    _onlySet =
      config.only.map(list => readFile(list).trim.split(LINE_SEP).toSet)
    config.msgdir.foreach(cleanDir)

    val (etestMap, ttestMap, originals) = input

    // Generate name->original map
    originals.foreach(s => originalMap += (s.name -> s.code))

    // do test for engines
    val engineResult = etestMap.map((e, tests) => {
      e -> tests
        .filterNot(test => skip(test) || doConformTest(e, false, test))
        .map(_.name)
    })

    // do test for transpilers
    val transResult = ttestMap.map((t, tests) => {
      t -> tests
        .filterNot(test => skip(test) || doConformTest(t, true, test))
        .map(_.name)
    })

    val result = engineResult ++ transResult

    // dump result as JSON
    dumpJson(result.map(_.toString -> _), s"$CONFORMTEST_LOG_DIR/fails.json")
    dumpJson(
      bugStat.map(_.toString -> _),
      s"$CONFORMTEST_LOG_DIR/bug-stat.json",
    )

    // return result
    result

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------

  private def debug(a: Any): Unit =
    if (_config.debug) println(a)

  // get the original code by name
  private var originalMap: Map[String, String] = Map()

  // bug stat
  private var bugStat: Map[Target, Map[String, Int]] = Map()

  // skip the test if this test is not on the only-list.
  private def skip(script: Script): Boolean =
    _onlySet.map(noSkips => !noSkips.contains(script.name)).getOrElse(false)

  // do conform test
  private def doConformTest(
    target: Target,
    isTrans: Boolean,
    script: Script,
  ): Boolean =
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
    if (!pass) {
      val detail =
        if (!sameExitTag) then
          s"[Exit Tag Mismatch]$LINE_SEP > Expected $exitTagRaw but got $concreteExitTag"
        else s"[Assertion Fail]$LINE_SEP > $stdout"

      val db = s"$RESOURCE_DIR/bugs/${target.name}/${target.version}"
      val original = originalMap(name)

      val tag = tagFinder(target, db, original, code, detail)

      val msg = s"""Test fail for `$target`: $name
                   |TAG: $tag
                   |$detail
                   |""".stripMargin

      debug(original)
      debug(msg)

      _config.msgdir.foreach(dir =>
        val path = s"$dir/$name.msg"
        val orig = optional(readFile(path)).getOrElse(original)
        dumpFile(orig + LINE_SEP + msg, path),
      )

      if (_config.saveBugs)
        val data = s"$original$LINE_SEP/* $msg */"
        tag match {
          case NewBug(_) => appendFile(data, s"$db/TODO")
          case TodoBug(name, _) =>
            dumpFile(data.replace("TODO", "NEW"), s"$db/TODO/$name")
          case _ =>
        }

      val origStat = bugStat.getOrElse(target, Map())
      val origCount = origStat.getOrElse(tag._tag, 0)
      val newStat = origStat + (tag._tag -> (origCount + 1))
      bugStat += (target -> newStat)
    }

    pass

  private val exitTagPattern = "(?<=\\[EXIT\\] )[^\n\r]*".r
  private val errorPattern = "[\\w]+Error(?=: )".r

  private def engineErrorResolver(engineError: Throwable): (ExitTag, String) =
    engineError match {
      case e: TimeoutException     => (TimeoutTag, "")
      case e @ NoCommandError(cmd) => throw e
      case e =>
        val msg = e.getMessage
        val tag = errorPattern.findFirstIn(msg) match {
          case Some(name)                             => ThrowErrorTag(name)
          case _ if msg.contains("TRANSPILE_FAILURE") => TranspileFailTag
          case _ => ThrowValueTag(esmeta.state.Str(msg))
        }
        (tag, "")
    }

  trait Bug(val _tag: String)
  case class KnownBug(val tag: String) extends Bug(tag) {
    override def toString = s"KNOWN-$tag"
  }
  case class TodoBug(val name: String, val tag: String) extends Bug(tag) {
    override def toString = s"TODO-$tag"
  }
  case class NewBug(val tag: String) extends Bug(tag) {
    override def toString = s"NEW-$tag"
  }

  private def tagFinder(
    target: Target,
    db: String,
    original: String,
    code: String,
    detail: String,
  ): Bug =
    val Target(targetName, version, _) = target

    // search if this bug is already known
    val buggies = walkTree(db).filter(jsFilter)
    val knownTag = buggies.foldLeft[Bug](NewBug("YET"))((cur, buggy) =>
      if (cur != NewBug("YET")) cur
      else if sameScript(readFile(buggy.getPath), original) then
        val dirname = buggy.getParentFile.getName
        if dirname == "TODO" then TodoBug(buggy.getName, "YET")
        else KnownBug(dirname)
      else cur,
    )

    // apply heuristic to guess tag of this bug
    val tag = manualRule.foldLeft(knownTag)((cur, rule) =>
      cur match {

        case NewBug("YET") =>
          val Array(tag, codePattern, msgPattern) = rule
          if (code.contains(codePattern) && detail.contains(msgPattern))
            NewBug(tag)
          else
            cur
        case TodoBug(file, "YET") =>
          val Array(tag, codePattern, msgPattern) = rule
          if (code.contains(codePattern) && detail.contains(msgPattern))
            TodoBug(file, tag)
          else
            cur
        case _ => cur
      },
    )

    tag

  private def sameScript(script1: String, script2: String): Boolean =
    (script1.split(LINE_SEP) zip script2.split(LINE_SEP)).forall(_ == _)

  private def appendFile(data: Any, dir: String) =
    val names = listFiles(dir).map(_.getName.dropRight(3).toInt).sorted
    val emptyIdx =
      names.zipWithIndex.find(p => p._1 != p._2).map(_._2).getOrElse(names.size)
    dumpFile(data, s"$dir/$emptyIdx.js")

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
      "only",
      StrOption((c, s) => c.only = Some(s)),
      "file that contains test names to run only",
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
    var only: Option[String] = None,
    var saveBugs: Boolean = false,
  )
}
