package esmeta.phase

import esmeta.*
import esmeta.error.*
import esmeta.es.util.injector.ConformTest.*
import esmeta.es.util.injector.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.util.BaseUtils.*
import esmeta.util.JSEngine

type Result = Map[String, List[String]]

/** `conform-test` phase */
case object ConformTest extends Phase[Unit, (Result, Result)] {
  val name = "conform-test"
  val help = "Perform conformance test for an ECMAScript Engine or a transpiler"

  private var _config: Config = null

  def apply(
    _unit: Unit,
    cmdConfig: CommandConfig,
    config: Config,
  ): (Result, Result) =
    _config = config

    // validate the target scripts
    val dirname = getFirstFilename(cmdConfig, this.name)
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
      script = readFile(s"$dirname/$file")
      testfile = file + ".test" // guaranteed to exist
      test = readFile(s"$dirname/$testfile")
      engineResult = engineConformTest(script, test, engines)
      transResult = transConformTest(script, test, transpilers)
      if (!engineResult.isEmpty || !transResult.isEmpty)
    } yield (file, engineResult, transResult)

    // transpose the result and colelct result per engine/transpiler
    scriptResult.foldLeft[(Result, Result)]((Map(), Map())) {
      case ((engineResult, transResult), (file, engineFails, transFails)) =>
        (
          updateResult(engineResult, engineFails, file),
          updateResult(transResult, transFails, file),
        )
    }

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

  private def engineConformTest(
    script: String,
    test: String,
    engines: List[String],
  ) =
    val exitTagPattern = "(?<=\\[EXIT\\] )[^\n\r]*".r
    val exitTagRaw = exitTagPattern.findFirstIn(test).get
    val exitTag = ExitTag(exitTagRaw).get
    val isNormal = exitTag == NormalTag
    val src = test.replace(placeholder, script)
    engines.filter(e => {
      val (concreteExitTag, stdout) =
        JSEngine
          .runUsingBinary(e, src)
          .map((NormalTag, _))
          .recover(_ match {
            case e: TimeoutException => (TimeoutTag, "")
            case e =>
              val msg = e.getMessage
              val errorPattern = "[\\w]+Error(?=: )".r
              val tag = errorPattern.findFirstIn(msg) match {
                case Some(name)                          => ThrowErrorTag(name)
                case _ if msg.contains(transpileFailure) => TranspileFailTag
                case _ => ThrowValueTag(esmeta.state.Str(msg))
              }
              (tag, "")
          })
          .get

      val sameExitTag = exitTag.equivalent(concreteExitTag)
      val pass = sameExitTag && stdout.isEmpty

      if (!pass) {
        val msg =
          if (!sameExitTag) then
            s"[Exit Tag Mismatch]$LINE_SEP > Expected $exitTagRaw but got $concreteExitTag"
          else s"[Assertion Fail]$LINE_SEP > $stdout"

        val category = manualRule.foldLeft("YET")((cur, rule) =>
          cur match {
            case "YET" =>
              val Array(tag, codePattern, msgPattern) = rule
              if (script.contains(codePattern) && msg.contains(msgPattern))
                tag
              else
                cur
            case _ => cur
          },
        )

        if (_config.debug)
          println(s"Test fail for `$e`")
          println(s"TAG: $category")
          println(msg)
          println()

        _config.msgdir.foreach(_ => ???)
      }

      !pass
    })

  private def transConformTest(
    script: String,
    test: String,
    transpilers: List[String],
  ) =
    transpilers.filter(t => {
      false
    })

  private val transpileFailure = "FAILURE"

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
