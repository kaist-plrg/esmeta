package esmeta.es.util.fuzzer

import esmeta.FUZZ_LOG_DIR
import esmeta.cfg.CFG
import esmeta.es.*
import esmeta.es.util.*
import esmeta.es.util.mutator.*
import esmeta.es.util.synthesizer.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import java.io.PrintWriter
import java.util.concurrent.TimeoutException

/** ECMAScript program fuzzer with ECMA-262 */
object Fuzzer:
  def apply(
    cfg: CFG,
    log: Option[Int] = Some(600), // default logging interval is 10 minutes.
    stdOut: Boolean = false,
    timeLimit: Option[Int] = None, // time limitation for each evaluation
    trial: Option[Int] = None, // `None` denotes no bound
    conformtest: Boolean = false,
  ): Coverage =
    new Fuzzer(cfg, log, stdOut, timeLimit, trial, conformtest).result

/** extensible helper of ECMAScript program fuzzer with ECMA-262 */
class Fuzzer(
  cfg: CFG,
  log: Option[Int] = Some(600), // default logging interval is 10 minutes.
  stdOut: Boolean = false,
  timeLimit: Option[Int] = None, // time limitation for each evaluation
  trial: Option[Int] = None, // `None` denotes no bound
  conformtest: Boolean = false,
) {

  /** generated ECMAScript programs */
  lazy val result: Coverage =
    println("- initializing program pool...")
    for (code <- synthesizer.initPool)
      add(code)

    println("- repeatedly trying to fuzz new programs to increase coverage...")
    log.map(_ => {
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
    log.map(_ => {
      logging
      nf.close
    })

    cov

  /** current program pool */
  def pool: Set[Script] = cov.minimalScripts

  /** one trial to fuzz a new program to increase coverage */
  def fuzz: Boolean = optional {
    iter += 1
    for (bound <- log) {
      val seconds = bound * 1000
      if (interval > seconds) {
        logging
        startInterval += seconds
      }
    }
    val target = choose(pool)
    val mutated = mutator(target.ast)
    val code = mutated.toString(grammar)
    for (_ <- log) {
      val simpleCode = if (code.length > 100) code.take(100) + "..." else code
      if (stdOut) {
        clearLine
        print(s"[$iter] (${Time(duration).simpleString}) $simpleCode")
      }
    }
    add(code)
  }.getOrElse(false)

  /** add new program */
  def add(code: String): Boolean = optional {
    if (visited contains code) false
    else {
      visited += code
      if (!ValidityChecker(code)) false
      else {
        val script = toScript(code)
        val (initSt, exitSt, updated) = cov.runAndCheck(script)
        if (conformtest) cov.doConformTest(initSt, exitSt)
        updated
      }
    }
  }.getOrElse(false)

  /** ECMAScript grammar */
  val grammar = cfg.grammar
  val scriptParser = cfg.scriptParser

  /** coverage */
  val cov: Coverage = Coverage(cfg, timeLimit)

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
    cov.dumpTo(FUZZ_LOG_DIR, withMsg = false)
  def addRaw(data: Any*): Unit =
    val raw = data.mkString("\t")
    if (stdOut) clearLine
    println(raw)
    nf.println(raw)
    nf.flush
  private def clearLine: Unit = print("\r" + (" " * 150) + "\r")
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
