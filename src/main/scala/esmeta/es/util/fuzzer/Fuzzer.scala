package esmeta.es.util.fuzzer

import esmeta.error.*
import esmeta.es.*
import esmeta.es.util.*
import esmeta.es.util.injector.*
import esmeta.es.util.mutator.*
import esmeta.es.util.synthesizer.*
import esmeta.state.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import esmeta.{ESMeta, FUZZ_LOG_DIR, LINE_SEP}
import io.circe.*, io.circe.syntax.*
import java.io.PrintWriter
import scala.collection.mutable.{ListBuffer, Map => MMap}
import scala.util._

/** ECMAScript program fuzzer with ECMA-262 */
object Fuzzer {
  def apply(
    logInterval: Option[Int] = Some(600), // default is 10 minutes.
    debug: Int = NO_DEBUG, // 2: all, 1: partial, 0: no-debug
    stdOut: Boolean = false,
    timeLimit: Option[Int] = None, // time limitation for each evaluation
    trial: Option[Int] = None, // `None` denotes no bound
    conformTest: Boolean = false,
    synK: Option[Int] = None,
  ): Coverage = new Fuzzer(
    logInterval,
    debug,
    stdOut,
    timeLimit,
    trial,
    conformTest,
    synK,
  ).result

  // debugging levels
  val ALL = 2
  val PARTIAL = 1
  val NO_DEBUG = 0
}

/** extensible helper of ECMAScript program fuzzer with ECMA-262 */
class Fuzzer(
  logInterval: Option[Int] = Some(600), // default is 10 minutes.
  debug: Int = NO_DEBUG, // 2: all, 1: partial, 0: no
  stdOut: Boolean = false,
  timeLimit: Option[Int] = None, // time limitation for each evaluation
  trial: Option[Int] = None, // `None` denotes no bound
  conformTest: Boolean = false,
  synK: Option[Int] = None,
) {
  import Fuzzer.*

  /** generated ECMAScript programs */
  lazy val result: Coverage =
    logInterval.map(_ => {
      // start logging
      mkdir(logDir, remove = true)
      createSymlink(symlink, logDir, overwrite = true)
      dumpFile(ESMeta.currentVersion, s"$logDir/version")
      dumpFile(getSeed, s"$logDir/seed")
      dumpFile(JSEngine.defaultEngineToString, s"$logDir/default-engine")
      genSummaryHeader
      genStatHeader(selector.names, selStatTsv)
      genStatHeader(mutator.names, mutStatTsv)
    })
    time(
      s"- initializing program pool with ${initPool.size} programs", {
        for {
          (synthesizer, rawCode) <- initPool
          code <- optional(scriptParser.from(rawCode).toString(grammar))
        } {
          debugging(f"[${synthesizer.name}%-30s] $code")
          add(code)
        }
      },
    )
    println(s"- the initial program pool consists of ${pool.size} programs.")
    time(
      "- repeatedly trying to fuzz new programs to increase coverage", {
        logInterval.map(_ => {
          startTime = System.currentTimeMillis
          startInterval = System.currentTimeMillis
          logging
        })
        trial match
          case Some(count) => for (_ <- Range(0, count)) fuzz
          case None        => while (true) fuzz
      },
    )

    // finish logging
    logInterval.map(_ => {
      logging
      summaryTsv.close
      selStatTsv.close
      mutStatTsv.close
    })

    cov

  /** current program pool */
  def pool: Set[Script] = cov.minimalScripts

  /** one trial to fuzz a new program to increase coverage */
  def fuzz: Boolean =
    iter += 1
    debugging(("-" * 40) + f"  iter: $iter%10d  " + ("-" * 40))
    for (bound <- logInterval) {
      val seconds = bound * 1000
      if (interval > seconds) {
        logging
        startInterval += seconds
      }
    }
    val (selectorName, script, condView, nearest) = selector(pool, cov)
    val selectorInfo = selectorName + condView.map(" - " + _).getOrElse("")
    val code = script.code
    debugging(f"[$selectorInfo%-30s] $code")

    val (mutatorName, mutated) = mutator(code, condView, nearest)
    val mutatedCode = mutated.toString(grammar)
    debugging(f"----- $mutatorName%-20s-----> $mutatedCode")

    val result = add(mutatedCode)
    update(selectorName, selectorStat, result)
    update(mutatorName, mutatorStat, result)
    result

  /** add new program */
  def add(code: String): Boolean =
    val result = Try {
      if (visited contains code)
        fail("ALREADY VISITED")
      visited += code
      if (!ValidityChecker(code))
        fail("INVALID PROGRAM")
      val script = toScript(code)
      val (initSt, exitSt, interp, updated, covered) = cov.runAndCheck(script)
      if (conformTest) doConformTest(initSt, exitSt, interp)
      if (!updated) fail("NO UPDATE")
      covered
    }
    debugging(f" ${"COVERAGE RESULT"}%30s: ", newline = false)
    result match {
      case Success(covered)             => debugging(passMsg("")); covered
      case Failure(e: TimeoutException) => debugging(failMsg("TIMEOUT")); false
      case Failure(e: NotSupported) =>
        debugging(failMsg("NOT SUPPORTED")); false
      case Failure(e) =>
        e.getMessage match
          case "ALREADY VISITED" | "INVALID PROGRAM" if debug == PARTIAL =>
            debugClean
          case msg =>
            debugging(failMsg(msg))
            debugFlush
        false
    }

  // conformance check counter for engines
  val engineMap: MMap[Coverage.NodeView, Counter] = MMap()

  // conformance check counter for transpilers
  val transMap: MMap[Coverage.NodeView, Counter] = MMap()

  // a pass-or-fail counter
  case class Counter(pass: Int = 0, fail: Int = 0)
  def update[T](t: T, map: MMap[T, Counter], pass: Boolean): Unit =
    val Counter(p, f) = map.getOrElse(t, Counter())
    val updated = if (pass) Counter(p + 1, f) else Counter(p, f + 1)
    map += t -> updated
  private def counterJson[T: Ordering](map: MMap[T, Counter]): Json =
    JsonObject(
      (for ((condView, Counter(pass, fail)) <- map.toList.sortBy(_._1)) yield {
        val key = condView.toString
        val obj = JsonObject(
          "pass" -> pass.asJson,
          "fail" -> fail.asJson,
        ).asJson
        key -> obj
      }): _*,
    ).asJson

  // all meaningful tests
  private val failedTests: ListBuffer[(String, ConformTest)] = ListBuffer()
  private val transFailedTests: ListBuffer[(String, ConformTest)] = ListBuffer()
  private def doConformTest(
    initSt: State,
    finalSt: State,
    interp: Coverage.Interp,
  ): Unit =
    val code = initSt.sourceText.get
    val (test, transTest) = ConformTest.createTestPair(initSt, finalSt)
    val d = if (iter == 0) 0 else duration
    val comment = List(
      s"// generated at iteration $iter",
      s"// took $d ms (${Time(d).simpleString})",
    ).mkString("", LINE_SEP, LINE_SEP)
    test.comment = comment
    transTest.comment = comment
    // conformance check for engines
    val pass = test.isPass
    debugging(f" ${"GRAAL-JS CONFORMANCE RESULT"}%30s: ", newline = false)
    if (!pass) failedTests.addOne(code, test)
    interp.touchedNodeViews.keys.map(update(_, engineMap, pass))
    debugging(if (test.isPass) passMsg("") else failMsg(""))
    // conformance check for transpilers
    val transPass = transTest.isPass
    debugging(f" ${"BABEL TRANSPILATION RESULT"}%30s: ", newline = false)
    if (!transPass) transFailedTests.addOne(code, transTest)
    interp.touchedNodeViews.keys.map(update(_, transMap, transPass))
    debugging(if (transTest.isPass) passMsg("") else failMsg(""))

  /** ECMAScript grammar */
  val grammar = cfg.grammar
  val scriptParser = cfg.scriptParser

  /** coverage */
  val cov: Coverage = Coverage(timeLimit, synK)

  /** target selector */
  val selector: TargetSelector = WeightedSelector(
    RandomSelector -> 2,
    BranchSelector -> 8,
  )

  /** selector stat */
  val selectorStat: MMap[String, Counter] = MMap()

  /** mutator */
  val mutator: Mutator = WeightedMutator(
    RandomMutator() -> 6,
    StatementInserter() -> 1,
    NearestMutator() -> 3,
  )

  /** mutator stat */
  val mutatorStat: MMap[String, Counter] = MMap()

  /** initial pool */
  val initPool =
    SimpleSynthesizer.initPool.map(SimpleSynthesizer -> _) ++
    BuiltinSynthesizer.initPool.map(BuiltinSynthesizer -> _)

  lazy val logDir: String = s"$FUZZ_LOG_DIR/fuzz-$dateStr"
  lazy val symlink: String = s"$FUZZ_LOG_DIR/recent"

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  // current iteration count
  private var iter: Int = 0

  // current id
  private var idCounter: Long = 0
  private def nextId: Long = { val id = idCounter; idCounter += 1; id }

  // evaluation start time
  private var startTime: Long = 0L
  private def duration: Long = System.currentTimeMillis - startTime
  private var startInterval: Long = 0L
  private def interval: Long = System.currentTimeMillis - startInterval

  // conversion from code string to `Script` object
  private def toScript(code: String): Script = Script(code, s"$nextId.js")

  // check if the added code is visited
  private var visited: Set[String] = Set()

  // indicating that add failed
  private def fail(msg: String) = throw Exception(msg)

  // debugging
  private var debugMsg = ""
  private def debugging(
    msg: String,
    newline: Boolean = true,
  ): Unit = if (debug == ALL) {
    if (newline) println(msg) else print(msg)
  } else if (debug > NO_DEBUG) {
    debugMsg += msg
    if (newline) debugMsg += LINE_SEP
  }
  private def debugClean: Unit = debugMsg = ""
  private def debugFlush: Unit = { print(debugMsg); debugClean }

  // dump conformance test counters
  private def dumpConformTestCounter(
    baseDir: String = logDir,
    withMsg: Boolean = true,
  ): Unit =
    dumpJson(
      name = if (withMsg) Some("conformance test counter") else None,
      data = counterJson(engineMap),
      filename = s"$baseDir/engine-count.json",
      space = true,
    )
    dumpJson(
      name = if (withMsg) Some("transpiled conformance test counter") else None,
      data = counterJson(transMap),
      filename = s"$baseDir/trans-count.json",
      space = true,
    )

  // dump failed conformance tests
  def dumpFailedConformTests(
    baseDir: String = logDir,
    withMsg: Boolean = true,
  ): Unit =
    /** dump failed engine conformance tests */
    rmdir(s"$baseDir/failed")
    val indexedFailedTests = failedTests.zipWithIndex
    val indexedTransFailedTests = transFailedTests.zipWithIndex
    type IndexedTest = ((String, ConformTest), Int)
    dumpDir[IndexedTest](
      name = if (withMsg) Some("failed conformance codes") else None,
      iterable = indexedFailedTests,
      dirname = s"$baseDir/failed",
      getName = { case ((c, t), i) => s"$i.js" },
      getData = { case ((c, t), i) => t.comment + USE_STRICT + c + LINE_SEP },
      remove = true,
    )
    dumpDir[IndexedTest](
      name = if (withMsg) Some("failed conformance tests") else None,
      iterable = indexedFailedTests,
      dirname = s"$baseDir/failed",
      getName = { case ((c, t), i) => s"$i.test.js" },
      getData = { case ((c, t), i) => t },
      remove = false,
    )
    dumpDir[IndexedTest](
      name =
        if (withMsg) Some("message for failed conformance tests") else None,
      iterable = indexedFailedTests,
      dirname = s"$baseDir/failed",
      getName = { case ((c, t), i) => s"$i.msg" },
      getData = { case ((c, t), i) => "TAG: " + t.category + LINE_SEP + t.msg },
      remove = false,
    )

    /** dump failed transpiler conformance tests */
    rmdir(s"$baseDir/trans-failed")
    dumpDir[IndexedTest](
      name = if (withMsg) Some("failed transpiled conformance codes") else None,
      iterable = indexedTransFailedTests,
      dirname = s"$baseDir/trans-failed",
      getName = { case ((c, t), i) => s"$i.js" },
      getData = { case ((c, t), i) => t.comment + USE_STRICT + c + LINE_SEP },
      remove = true,
    )
    dumpDir[IndexedTest](
      name = if (withMsg) Some("failed transpiled conformance tests") else None,
      iterable = indexedTransFailedTests,
      dirname = s"$baseDir/trans-failed",
      getName = { case ((c, t), i) => s"$i.test.js" },
      getData = { case ((c, t), i) => t },
      remove = false,
    )
    dumpDir[IndexedTest](
      name =
        if (withMsg) Some("message for failed transpiled conformance tests")
        else None,
      iterable = indexedTransFailedTests,
      dirname = s"$baseDir/trans-failed",
      getName = { case ((c, t), i) => s"$i.msg" },
      getData = { case ((c, t), i) => "TAG: " + t.category + LINE_SEP + t.msg },
      remove = false,
    )

  // generate headers
  private def genSummaryHeader =
    var header = Vector(
      "iter(#)",
      "time(ms)",
      "time(h:m:s)",
      "program(#)",
      "minimal(#)",
      "node(#)",
      "branch(#)",
    )
    synK.map(k => header ++= Vector(s"$k-syn-node(#)", s"$k-syn-branch(#)"))
    header ++= Vector("target-conds(#)")
    if (conformTest) header ++= Vector("conform-bug(#)", "trans-bug(#)")
    addRow(header)
  private def genStatHeader(keys: List[String], nf: PrintWriter) =
    var header1 = Vector("iter(#)")
    var header2 = Vector("-")
    keys.foreach(k => {
      header1 ++= Vector(k, "-", "-", "-")
      header2 ++= Vector("pass", "fail", "total", "ratio")
    })
    addRow(header1, nf)
    addRow(header2, nf)

  // dump selector and mutator stat
  private def dumpStat(
    keys: List[String],
    stat: MMap[String, Counter],
    tsv: PrintWriter,
  ): Unit =
    var row = Vector[Any](iter)
    keys.foreach(k => {
      val Counter(pass, fail) = stat.getOrElse(k, Counter())
      val total = pass + fail
      val ratio = optional((pass * 10000) / total / 100.0).getOrElse(0.0)
      row ++= Vector(pass, fail, total, s"$ratio%")
    })
    addRow(row, tsv)

  // logging
  private def logging: Unit =
    val n = cov.nodeCov
    val b = cov.branchCov
    val d = duration
    val t = Time(d).simpleString
    val nv = cov.nodeViewCov
    val bv = cov.branchViewCov
    val cb = failedTests.size
    val tb = transFailedTests.size
    val tc = cov.targetCondSize
    var row = Vector(iter, d, t, visited.size, pool.size, n, b)
    if (synK.isDefined) row ++= Vector(nv, bv)
    row ++= Vector(tc)
    if (conformTest) row ++= Vector(cb, tb)
    addRow(row)
    // dump coveragge
    cov.dumpToWithDetail(logDir, withMsg = false)
    // dump failed conformance tests
    if (conformTest)
      dumpConformTestCounter(logDir, false)
      dumpFailedConformTests(logDir, false)
    // dump selector and mutator stat
    dumpStat(selector.names, selectorStat, selStatTsv)
    dumpStat(mutator.names, mutatorStat, mutStatTsv)
  private def addRow(data: Iterable[Any], nf: PrintWriter = summaryTsv): Unit =
    val row = data.mkString("\t")
    if (stdOut) println(row)
    nf.println(row)
    nf.flush
  private lazy val summaryTsv: PrintWriter = getPrintWriter(
    s"$logDir/summary.tsv",
  )
  private lazy val selStatTsv: PrintWriter = getPrintWriter(
    s"$logDir/selector-stat.tsv",
  )
  private lazy val mutStatTsv: PrintWriter = getPrintWriter(
    s"$logDir/mutation-stat.tsv",
  )

}
