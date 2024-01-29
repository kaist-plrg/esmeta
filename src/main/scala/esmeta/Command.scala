package esmeta

import esmeta.phase.*
import esmeta.util.ArgParser

/** commands
  *
  * @tparam Result
  *   the result typeof command
  */
sealed abstract class Command[Result](
  /** command name */
  val name: String,

  /** phase list */
  val pList: PhaseList[Result],
) {
  override def toString: String = pList.toString

  /** help message */
  def help: String

  /** help message */
  def examples: List[String]

  /** show the final result */
  def showResult(res: Result): Unit = println(res)

  /** target name */
  def targetName: String = ""

  /** need target */
  def needTarget: Boolean = targetName != ""

  /** run command with command-line arguments */
  def apply(args: List[String]): Result =
    val cmdConfig = CommandConfig(this)
    val parser = ArgParser(this, cmdConfig)
    val runner = pList.getRunner(parser)
    parser(args)
    ESMeta(this, runner(_), cmdConfig)

  /** a list of phases without specific IO types */
  def phases: Vector[Phase[_, _]] = pList.phases

  /** append a phase to create a new phase list */
  def >>[R](phase: Phase[Result, R]): PhaseList[R] = pList >> phase
}

/** base command */
case object CmdBase extends Command("", PhaseNil) {
  val help = "does nothing."
  val examples = Nil
}

/** `help` command */
case object CmdHelp extends Command("help", CmdBase >> Help) {
  val help = "shows help messages."
  val examples = List(
    "esmeta help                  // show help message.",
    "esmeta help extract          // show help message of `extract` command.",
  )
  override val targetName = "[<command>]"
  override val needTarget = false
}

// -----------------------------------------------------------------------------
// Mechanized Specification Extraction
// -----------------------------------------------------------------------------
/** `extract` command */
case object CmdExtract extends Command("extract", CmdBase >> Extract) {
  val help = "extracts specification model from ECMA-262 (spec.html)."
  val examples = List(
    "esmeta extract                           // extract current version.",
    "esmeta extract -extract:target=es2022    // extract es2022 version.",
    "esmeta extract -extract:target=868fe7a   // extract 868fe7a hash version.",
  )
}

/** `compile` command */
case object CmdCompile extends Command("compile", CmdExtract >> Compile) {
  val help = "compiles a specification to an IR program."
  val examples = List(
    "esmeta compile                        # compile spec to IR program.",
    "esmeta compile -extract:target=es2022 # compile es2022 spec to IR program",
  )
}

/** `build-cfg` command */
case object CmdBuildCFG extends Command("build-cfg", CmdCompile >> BuildCFG) {
  val help = "builds a control-flow graph (CFG) from an IR program."
  val examples = List(
    "esmeta build-cfg                          # build CFG for spec.",
    "esmeta build-cfg -extract:target=es2022   # build CFG for es2022 spec.",
  )
}

// -----------------------------------------------------------------------------
// Analysis of ECMA-262
// -----------------------------------------------------------------------------
/** `tycheck` command */
case object CmdTypeCheck extends Command("tycheck", CmdBuildCFG >> TypeCheck) {
  val help = "performs a type analysis of ECMA-262."
  val examples = List(
    "esmeta tycheck                              # type check for spec.",
    "esmeta tycheck -tycheck:target=\".*ToString\" # type check with targets",
    "esmeta tycheck -extract:target=es2022       # type check for es2022 spec.",
  )
}

// -----------------------------------------------------------------------------
// Interpreter & Double Debugger for ECMAScript
// -----------------------------------------------------------------------------
/** `parse` command */
case object CmdParse extends Command("parse", CmdExtract >> Parse) {
  val help = "parses an ECMAScript file."
  val examples = List(
    "esmeta parse a.js                         # parse a.js file.",
    "esmeta parse a.js -extract:target=es2022  # parse with es2022 spec.",
    "esmeta parse a.js -parse:debug            # parse in the debugging mode.",
  )
  override val targetName = "<js>"
}

/** `eval` command */
case object CmdEval extends Command("eval", CmdBuildCFG >> Eval) {
  val help = "evaluates an ECMAScript file."
  val examples = List(
    "esmeta eval a.js                         # eval a.js file.",
    "esmeta eval a.js -extract:target=es2022  # eval with es2022 spec.",
    "esmeta eval a.js -eval:log               # eval in the logging mode.",
  )
  override val targetName = "<js>"
}

/** `web` command */
case object CmdWeb extends Command("web", CmdBuildCFG >> Web) {
  val help = "starts a web server for an ECMAScript double debugger."
  val examples = List(
    "esmeta web    # turn on the server (Use with `esmeta-debugger-client`).",
  )
}

// -----------------------------------------------------------------------------
// Tester for Test262 (ECMAScript Test Suite)
// -----------------------------------------------------------------------------
/** `test262-test` command */
case object CmdTest262Test
  extends Command("test262-test", CmdBuildCFG >> Test262Test) {
  val help = "tests Test262 tests with harness files (default: tests/test262)."
  val examples = List(
    "esmeta test262-test                                           # all ",
    "esmeta test262-test tests/test262/test/built-ins/Map/map.js   # file",
    "esmeta test262-test tests/test262/test/language/expressions   # directory",
  )
  override val targetName = "<js|dir>*"
  override val needTarget = false
}

// -----------------------------------------------------------------------------
// ECMAScript Helper Modules
// -----------------------------------------------------------------------------
/** `inject` command */
case object CmdInject extends Command("inject", CmdBuildCFG >> Inject) {
  val help = "injects assertions to check final state of an ECMAScript file."
  val examples = List(
    "esmeta inject a.js                               # inject assertions.",
    "esmeta inject a.js -inject:defs -inject:out=b.js # dump with definitions.",
  )
  override val targetName = "<js>"
}

/** `mutate` command */
case object CmdMutate extends Command("mutate", CmdBuildCFG >> Mutate) {
  def help = "mutates an ECMAScript program."
  val examples = List(
    "esmeta mutate a.js                           # mutate ECMAScript program.",
    "esmeta mutate a.js -mutate:out=b.js          # dump the mutated program.",
    "esmeta mutate a.js -mutate:mutator=random    # use random mutator.",
  )
  override val targetName = "<js>"
}

/** `transcheck` command */
case object CmdTransCheck
  extends Command("transcheck", CmdBuildCFG >> TransCheck) {
  val help = "transpiles and validates an ECMAScript program."
  val examples = List(
    "esmeta transcheck a.js    # transpile a.js file and validate the result.",
  )
  override val targetName = "<js>"
}

/** `fuzz` command */
case object CmdFuzz extends Command("fuzz", CmdBuildCFG >> Fuzz) {
  val help = "generate ECMAScript programs for fuzzing."
  val examples = List(
    "esmeta fuzz                 # generate ECMAScript programs for fuzzing",
    "esmeta fuzz -fuzz:out=out   # dump the generated program to `out`",
  )
  override def showResult(cov: es.util.Coverage): Unit =
    println(s"- generated ${cov.size} ECMAScript programs.")
    println(cov)
}

/** `gen-test` command */
case object CmdGenTest extends Command("gen-test", CmdBase >> GenTest) {
  val help =
    "generate conform tests for an ECMAScript engine or a transpiler based."
  val examples = List(
    "esmeta gen-test codedir assertiondir       # perform conform test using script and test in dir",
    "esmeta gen-test                            # perform conform test using most recent fuzzing result",
    "esmeta gen-test get-test:engines=\"d8,js\" # perform conformtest for d8 and js",
  )
  override def showResult(
    testMapPair: (
      Map[esmeta.js.Target, Iterable[esmeta.es.Script]],
      Map[esmeta.js.Target, Iterable[esmeta.es.Script]],
      Iterable[esmeta.es.Script],
    ),
  ): Unit =
    val (etestMap, ttestMap, _) = testMapPair
    etestMap.foreach {
      case (engine, tests) =>
        println(s"${tests.size} tests generated for the engine `$engine`.")
    }
    ttestMap.foreach {
      case (trans, tests) =>
        println(s"${tests.size} tests generated for the transpiler `$trans`.")
    }
}

/** `comform-test` command */
case object CmdConformTest
  extends Command("conform-test", CmdGenTest >> ConformTest) {
  val help = "perform conform test for an ECMAScript engine or a transpiler."
  val examples = List(
    "esmeta comform-test                     # perform conform test",
  )
  override def showResult(
    fails: Map[esmeta.js.Target, Iterable[String]],
  ): Unit =
    fails.foreach {
      case (e, fails) =>
        println(s"failing tests for `$e`: ")
        fails.foreach(f => println(s"  $f"))
    }
}

/** `delta-debug` command */
case object CmdDeltaDebug
  extends Command("delta-debug", CmdBuildCFG >> DeltaDebug) {
  def help =
    "finds the minimal configuration of an ECMAScript program which induces a failure in conform test of an ECMAScript engine or a transpiler"
  val examples = List(
    "esmeta delta-debug a.js -mutate:impl=swc                    # minimize ECMAScript program which fails testing swc.",
    "esmeta delta-debug a.js -mutate:impl=sm -mutate:out=b.js    # dump the minimized programs in b.js.",
  )
  override val targetName = "<js>"
}

/** `aggregate` command */
case object CmdAggregate
  extends Command("aggregate", CmdBuildCFG >> Aggregate) {
  def help =
    "aggregate the failing comform test cases"
  val examples = List(
    s"esmeta ${this.name} minimal fails.json   # aggregate.",
  )
}

/** `localize` command */
case object CmdLocalize extends Command("localize", CmdBuildCFG >> Localize) {
  val help = "localize bug using given jsons"
  val examples = List(
    "esmeta localize node-coverage.json minimal-touch-node.json fails.json # localize using given jsons",
  )
  override def showResult(
    result: Map[String, Map[String, Seq[(Any, Double)]]],
  ): Unit =
    result.foreach((target, failMap) => {
      println(s"[Localization result for `$target`]")
      failMap.foreach((fail, locs) => {
        println(s"[[$fail]]")
        val ones = locs.filter(_._2 == 1.0)
        (if ones.isEmpty then locs else ones).foreach((location, score) =>
          println(s"  $location: $score"),
        )
      })
    })
}

/** `categorize` command */
case object CmdCategorize extends Command("categorize", CmdBase >> Categorize) {
  val help = "categorize bug"
  val examples = List(
    "esmeta categorize minimal fails.json      # categorize",
  )
  override def showResult(result: Map[String, Map[String, Int]]): Unit =
    result.foreach((target, bugStat) => {
      println(s"[Categorization result for `$target`]")
      bugStat.toSeq.sorted.foreach((bug, count) => {
        println(s"$bug: $count")
      })
    })
}

/** `handle-coverage` command */
case object CmdHandleCoverage
  extends Command("handle-coverage", CmdBuildCFG >> HandleCoverage) {
  val help = "various operation on coverage"
  val examples = List(
    "esmeta handle-coverage -handle-coverage:lower node-coverage.json",
  )
  override def showResult(_unit: Unit) = ()
  override val targetName = "<json>"
}

// -----------------------------------------------------------------------------
// ECMAScript Static Analysis (Meta-Level Static Analysis)
// -----------------------------------------------------------------------------
/** `analyze` command */
case object CmdAnalyze extends Command("analyze", CmdBuildCFG >> Analyze) {
  val help = "analyzes an ECMAScript file using meta-level static analysis."
  val examples = List(
    "esmeta analyze a.js                         # analyze a.js file.",
    "esmeta analyze a.js -extract:target=es2022  # analyze with es2022 spec.",
    "esmeta analyze a.js -analyze:repl           # analyze in a REPL mode.",
  )
  override val targetName = "<js>"
}
