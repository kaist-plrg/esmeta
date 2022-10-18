package esmeta.phase

import esmeta.*
import esmeta.error.*
import esmeta.es.Script
import esmeta.es.util.injector.ConformTest.*
import esmeta.es.util.injector.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.util.BaseUtils.*
import esmeta.util.{JSEngine, JSTrans}

/** `conform-test` phase */
case object ConformTest
  extends Phase[
    (Map[String, Iterable[Script]], Map[String, Iterable[Script]]),
    Map[String, Iterable[String]],
  ] {
  val name = "conform-test"
  val help = "Perform conformance test for an ECMAScript Engine or a transpiler"

  private var _config: Config = null
  private var dirname = ""
  private type Target = String
  private type Input = Map[Target, Iterable[Script]]
  private type Result = Map[Target, Iterable[String]]

  def apply(
    testMapPair: (Input, Input),
    cmdConfig: CommandConfig,
    config: Config,
  ): Result =
    _config = config
    config.msgdir.foreach(cleanDir)

    val (etestMap, ttestMap) = testMapPair

    val engineResult = etestMap.map((e, tests) => {
      e -> tests.filterNot(test => doConformTest(e, false, test)).map(_.name)
    })

    val transResult = ttestMap.map((t, tests) => {
      t -> tests.filterNot(test => doConformTest(t, true, test)).map(_.name)
    })

    val result = engineResult ++ transResult

    // dump result as JSON
    dumpJson(result, s"$CONFORMTEST_LOG_DIR/fails.json")

    // return result
    result

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------

  private def doConformTest(
    target: String,
    isTrans: Boolean,
    script: Script,
  ): Boolean =
    val Script(code, name) = script
    if (_config.debug) println(s"Testing $target: $name...")

    // Obtain expected result
    val exitTagRaw = exitTagPattern.findFirstIn(code).get
    val exitTag = ExitTag(exitTagRaw).get
    val isNormal = exitTag == NormalTag

    // Obtain concrete result
    val (concreteExitTag, stdout) =
      if (!isTrans) {
        JSEngine
          .runUsingBinary(target, code)
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
            if (code.contains(codePattern) && detail.contains(msgPattern))
              tag
            else
              cur
          case _ => cur
        },
      )

      val msg = s"""Test fail for `$target`: $name
                   |TAG: $category
                   |$detail
                   |""".stripMargin

      if (_config.debug)
        println(msg)

      _config.msgdir.foreach(dir =>
        val path = s"$dir/$name.msg"
        val orig = optional(readFile(path)).getOrElse("")
        dumpFile(orig + msg + LINE_SEP, path),
      )
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
  )
  case class Config(
    var debug: Boolean = false,
    var msgdir: Option[String] = None,
  )
}
