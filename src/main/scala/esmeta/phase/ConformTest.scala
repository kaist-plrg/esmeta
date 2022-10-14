package esmeta.phase

import esmeta.*
import esmeta.error.*
import esmeta.es.util.injector.ConformTest.*
import esmeta.es.util.injector.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.util.BaseUtils.*
import esmeta.util.{JSEngine, JSTrans}

/** `conform-test` phase */
case object ConformTest extends Phase[Unit, Map[String, Seq[String]]] {
  val name = "conform-test"
  val help = "Perform conformance test for an ECMAScript Engine or a transpiler"

  private var _config: Config = null
  private var dirname = ""
  private type Result = Map[String, List[String]]

  def apply(_unit: Unit, cmdConfig: CommandConfig, config: Config): Result =
    _config = config
    config.msgdir.foreach(cleanDir)

    // validate the target scripts
    dirname = optional(getFirstFilename(cmdConfig, this.name))
      .getOrElse(s"$FUZZ_LOG_DIR/recent/minimal")
    val files = listFiles(dirname).filter(_.isFile).map(_.getName)
    if (files.isEmpty)
      warnUsage
    val fileMap = files.groupBy(getExt).map(_ -> _.toSet)
    validate(fileMap)

    // collect target engines and transpilers
    val engines = config.engines match {
      case None => List(JSEngine.defaultCmd("d8"), JSEngine.defaultCmd("js"))
      case Some(es) => es.split(";").toList
    }
    val transpilers = config.transpilers match {
      case None     => List("babel")
      case Some(ts) => ts.split(";").toList
    }

    // collect result per scritps
    val scriptResult = for {
      file <- fileMap("js")
      _ = if (_config.debug) println(s"========= Testing $file... ==========")
      // read files
      script = readFile(s"$dirname/$file")
      testfile = file + ".test" // guaranteed to exist
      test = readFile(s"$dirname/$testfile")
      // do test
      targets = engines.map(e => (e, false)) ++ transpilers.map(t => (t, true))
      result = doConformTest(file, script, test, targets).map(_._1)
      if (!result.isEmpty)
    } yield (file, result)

    // transpose the result and collect result per engine/transpiler
    val result = scriptResult.foldLeft[Result](Map()) {
      case (result, (file, fails)) =>
        updateResult(result, fails, file)
    }

    // dump result as JSON
    dumpJson(result, s"$CONFORMTEST_LOG_DIR/fails.json")

    // return result
    result

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------

  // warn about correct usage
  private def warnUsage =
    throw new Exception("""
      |Usage: Give directory name, that contains pairs of js and test files
      |   ex: sample.js sample.js.test""".stripMargin)

  // check if the given directory structure is valid
  private def validate(fileMap: Map[String, Set[String]]): Unit =
    def assert(b: Boolean) = if (!b) warnUsage
    assert(fileMap.size == 2)
    assert(fileMap.contains("js") && fileMap.contains("test"))
    assert(fileMap("js").map(_ + ".test") == fileMap("test"))

  private def doConformTest(
    file: String,
    script: String,
    test: String,
    targets: List[(String, Boolean)],
  ): List[(String, Boolean)] =
    val exitTagRaw = exitTagPattern.findFirstIn(test).get
    val exitTag = ExitTag(exitTagRaw).get
    val isNormal = exitTag == NormalTag
    val src = test.replace(placeholder, script)
    targets.filter((target, isTrans) => {
      val (concreteExitTag, stdout) = if (!isTrans) {
        JSEngine
          .runUsingBinary(target, src)
          .map((NormalTag, _))
          .recover(engineErrorResolver _)
          .get
      } else {
        JSTrans
          .transpileFileUsingBinary(target, s"$dirname/$file")
          .map(transpiled => {
            val transpiledSrc = test.replace(placeholder, transpiled)
            JSEngine
              .run(transpiledSrc)
              .map((NormalTag, _))
              .recover(engineErrorResolver _)
              .get
          })
          .getOrElse((TranspileFailTag, ""))
      }

      val sameExitTag = exitTag.equivalent(concreteExitTag)
      val pass = sameExitTag && stdout.isEmpty

      // generate fail message
      if (!pass) {
        val detail =
          if (!sameExitTag) then
            s"[Exit Tag Mismatch]$LINE_SEP > Expected $exitTagRaw but got $concreteExitTag"
          else s"[Assertion Fail]$LINE_SEP > $stdout"

        val category = manualRule.foldLeft("YET")((cur, rule) =>
          cur match {
            case "YET" =>
              val Array(tag, codePattern, msgPattern) = rule
              if (script.contains(codePattern) && detail.contains(msgPattern))
                tag
              else
                cur
            case _ => cur
          },
        )

        val msg = s"""Test fail for `$target`
                     |TAG: $category
                     |$detail
                     |""".stripMargin

        if (_config.debug)
          println(msg)

        _config.msgdir.foreach(dir =>
          val path = s"$dir/$file.msg"
          val orig = optional(readFile(path)).getOrElse("")
          dumpFile(orig + msg + LINE_SEP, path),
        )
      }

      !pass
    })

  private val exitTagPattern = "(?<=\\[EXIT\\] )[^\n\r]*".r
  private val errorPattern = "[\\w]+Error(?=: )".r
  private val transpileFailure = "FAILURE"

  private def engineErrorResolver(engineError: Throwable) =
    engineError match {
      case e: TimeoutException => (TimeoutTag, "")
      case e =>
        val msg = e.getMessage
        val tag = errorPattern.findFirstIn(msg) match {
          case Some(name) => ThrowErrorTag(name)
          case _          => ThrowValueTag(esmeta.state.Str(msg))
        }
        (tag, "")
    }

  private def updateResult(
    result: Result,
    ks: List[String],
    v: String,
  ): Result =
    ks.foldLeft(result) {
      case (r, k) =>
        r.updatedWith(k)(_ match {
          case None     => Some(List(v))
          case Some(vs) => Some(v :: vs)
        })
    }

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
      "engines",
      StrOption((c, s) => c.engines = Some(s)),
      "list of engines to test, separated by ;",
    ),
    (
      "transpilers",
      StrOption((c, s) => c.transpilers = Some(s)),
      "list of transpilers to test, separated by ;",
    ),
  )
  case class Config(
    var debug: Boolean = false,
    var msgdir: Option[String] = None,
    var engines: Option[String] = None,
    var transpilers: Option[String] = None,
  )
}
