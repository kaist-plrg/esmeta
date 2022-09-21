package esmeta.es.util.fuzzer

import esmeta.FUZZ_LOG_DIR
import esmeta.cfg.CFG
import esmeta.es.*
import esmeta.es.util.*
import esmeta.es.util.mutator.*
import esmeta.es.util.synthesizer.*
import esmeta.state.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import java.io.PrintWriter
import java.util.concurrent.TimeoutException
import scala.collection.mutable.{Set => MSet}

/** ECMAScript program fuzzer with ECMA-262 */
object Fuzzer:
  def apply(
    cfg: CFG,
    logInterval: Option[Int] = Some(600), // default is 10 minutes.
    debug: Boolean = false,
    stdOut: Boolean = false,
    timeLimit: Option[Int] = None, // time limitation for each evaluation
    trial: Option[Int] = None, // `None` denotes no bound
    conformTest: Boolean = false,
  ): Coverage = new Fuzzer(
    cfg,
    logInterval,
    debug,
    stdOut,
    timeLimit,
    trial,
    conformTest,
  ).result

/** extensible helper of ECMAScript program fuzzer with ECMA-262 */
class Fuzzer(
  cfg: CFG,
  logInterval: Option[Int] = Some(600), // default is 10 minutes.
  debug: Boolean = false,
  stdOut: Boolean = false,
  timeLimit: Option[Int] = None, // time limitation for each evaluation
  trial: Option[Int] = None, // `None` denotes no bound
  conformTest: Boolean = false,
) {

  /** generated ECMAScript programs */
  lazy val result: Coverage =
    println("- initializing program pool...")
    for (code <- synthesizer.initPool)
      if (debug) println(f"[${synthesizer.name}%-30s] $code")
      add(code)

    println("- repeatedly trying to fuzz new programs to increase coverage...")
    logInterval.map(_ => {
      // start logging
      mkdir(FUZZ_LOG_DIR)
      startTime = System.currentTimeMillis
      startInterval = System.currentTimeMillis
      addRaw(
        "iter(#)",
        "script(#)",
        "time(ms)",
        "node-cover(#)",
        "node-total(#)",
        "node-ratio(%)",
        "branch-cover(%)",
        "branch-total(%)",
        "branch-ratio(%)",
      )
      logging
    })
    trial match
      case Some(count) => for (_ <- Range(0, count)) fuzz
      case None        => while (true) fuzz

    // finish logging
    logInterval.map(_ => {
      logging
      nf.close
    })

    cov

  /** current program pool */
  def pool: Set[Script] = cov.minimalScripts

  /** one trial to fuzz a new program to increase coverage */
  def fuzz: Boolean = optional {
    iter += 1
    for (bound <- logInterval) {
      val seconds = bound * 1000
      if (interval > seconds) {
        logging
        startInterval += seconds
      }
    }
    val target = selector(pool, cov, cfg.grammar, debug)
    val mutated = mutator(target.ast)
    val code = mutated.toString(grammar)
    if (debug) println(f"----- ${mutator.name}%-20s-----> $code")
    add(code)
  }.getOrElse(false)

  /** add new program */
  def add(code: String): Boolean = optional {
    if (debug) print(" " * 25 + "RESULT: ")
    if (visited contains code) {
      if (debug) println(failMsg("ALREADY VISITED"))
      false
    } else {
      visited += code
      if (!ValidityChecker(code)) {
        if (debug) println(failMsg("INVALID PROGRAM"))
        false
      } else {
        val script = toScript(code)
        val (initSt, exitSt, updated) = cov.runAndCheck(script)
        if (debug) println(if (updated) passMsg("") else failMsg("NO UPDATE"))
        if (conformTest) doConformTest(initSt, exitSt)
        updated
      }
    }
  }.getOrElse(false)

  // all meaningful tests
  private val failedTests: MSet[(String, ConformTest)] = MSet()
  private val transFailedTests: MSet[(String, ConformTest)] = MSet()
  private def doConformTest(initSt: State, finalSt: State) =
    val code = initSt.sourceText.get
    val (test, transTest) = ConformTest.createTestPair(initSt, finalSt)
    if (!test.isPass) failedTests.add(code, test)
    if (!transTest.isPass) transFailedTests.add(code, transTest)

  /** ECMAScript grammar */
  val grammar = cfg.grammar
  val scriptParser = cfg.scriptParser

  /** coverage */
  val cov: Coverage = Coverage(cfg, timeLimit)

  /** target selector */
  val selector: TargetSelector = WeightedSelector(
    RandomSelector -> 2,
    BranchSelector -> 8,
  )

  /** mutator */
  val mutator: Mutator = RandomMutator(cfg.grammar)

  /** synthesizer */
  val synthesizer: Synthesizer = RandomSynthesizer(cfg.grammar)

  /** logging */
  def logging: Unit =
    val (n, nt) = cov.nodeCov
    val nr = percentString(n, nt)
    val (b, bt) = cov.branchCov
    val br = percentString(b, bt)
    addRaw(iter, pool.size, duration, n, nt, nr, b, bt, br)
    // dump coveragge
    cov.dumpTo(FUZZ_LOG_DIR, withMsg = false)
    // dump failed conformance tests
    if (conformTest) dumpFailedConformTests(FUZZ_LOG_DIR, false)

  /** dump failed conformance tests */
  def dumpFailedConformTests(
    baseDir: String = FUZZ_LOG_DIR,
    withMsg: Boolean = true,
  ): Unit =
    rmdir(s"$baseDir/failed")
    type Zipped = ((String, ConformTest), Int)
    dumpDir[Zipped](
      name = if (withMsg) Some("failed conformance codes") else None,
      iterable = failedTests.zipWithIndex,
      dirname = s"$baseDir/failed",
      getName = { case ((c, t), i) => s"$i.js" },
      getData = { case ((c, t), i) => c },
    )
    dumpDir[Zipped](
      name = if (withMsg) Some("failed conformance tests") else None,
      iterable = failedTests.zipWithIndex,
      dirname = s"$baseDir/failed",
      getName = { case ((c, t), i) => s"$i.test.js" },
      getData = { case ((c, t), i) => t },
    )
    rmdir(s"$baseDir/trans-failed")
    dumpDir[Zipped](
      name = if (withMsg) Some("failed transpiled conformance codes") else None,
      iterable = transFailedTests.zipWithIndex,
      dirname = s"$baseDir/trans-failed",
      getName = { case ((c, t), i) => s"$i.js" },
      getData = { case ((c, t), i) => c },
    )
    dumpDir[Zipped](
      name = if (withMsg) Some("failed transpiled conformance tests") else None,
      iterable = transFailedTests.zipWithIndex,
      dirname = s"$baseDir/trans-failed",
      getName = { case ((c, t), i) => s"$i.test.js" },
      getData = { case ((c, t), i) => t },
    )

  def addRaw(data: Any*): Unit =
    val raw = data.mkString("\t")
    if (stdOut) println(raw)
    nf.println(raw)
    nf.flush
  lazy val nf: PrintWriter = getPrintWriter(s"$FUZZ_LOG_DIR/summary.tsv")

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
  private def toScript(code: String, ast: Ast): Script =
    val script = Script(code, ast, nextId.toString, None)
    visited += code
    script
  private def toScript(code: String): Script =
    toScript(code, scriptParser.from(code))
  private def toScript(ast: Ast): Script =
    toScript(ast.toString(grammar), ast)
  private var visited: Set[String] = Set()
}
