package esmeta.es.util.fuzzer

import esmeta.cfg.CFG
import esmeta.es.*
import esmeta.es.util.*
import esmeta.es.util.mutator.*
import esmeta.es.util.synthesizer.*
import esmeta.util.BaseUtils.*
import java.util.concurrent.TimeoutException

/** ECMAScript program fuzzer with ECMA-262 */
object Fuzzer:
  def apply(
    cfg: CFG,
    log: Boolean = false,
    timeLimit: Option[Int] = None, // time limitation for each evaluation
    maxIter: Option[Int] = None, // `None` denotes no bound
  ): Set[String] = new Fuzzer(cfg, log, timeLimit, maxIter).result

/** extensible helper of ECMAScript program fuzzer with ECMA-262 */
class Fuzzer(
  cfg: CFG,
  log: Boolean = false,
  timeLimit: Option[Int] = None, // time limitation for each evaluation
  maxIter: Option[Int] = None, // `None` denotes no bound
) {

  /** generated ECMAScript programs */
  lazy val result: Set[String] =
    println("initializing program pools...")
    for (ast <- synthesizer.initPool) add(ast)
    println("repeatedly trying to fuzz new programs to increase coverage...")
    maxIter match
      case Some(count) => for (_ <- Range(0, count)) fuzz
      case None        => while (true) fuzz
    pool.toSet

  /** current program pool */
  def programs: Vector[String] = pool

  /** one trial to fuzz a new program to increase coverage */
  def fuzz: Boolean = optional {
    iter += 1
    val target = toScript(choose(pool))
    val ast = mutator(target.ast)
    val code = ast.toString(grammar)
    val updated =
      if (!visited.contains(code)) add(code, ast)
      else false
    updated
  }.getOrElse(false)

  /** add new program */
  def add(code: String, ast: Ast): Boolean = optional {
    val script = toScript(code, ast)
    val (st, updated) = cov.runAndCheck(script)
    if (updated)
      pool :+= code
      println(s"[$iter]: $code")
      println(cov)
    updated
  }.getOrElse(false)
  def add(code: String): Boolean = add(code, scriptParser.from(code))
  def add(ast: Ast): Boolean = add(ast.toString(grammar), ast)

  /** ECMAScript grammar */
  val grammar = cfg.grammar
  val scriptParser = cfg.scriptParser

  /** coverage */
  val cov: Coverage = Coverage(cfg, timeLimit)

  /** mutator */
  val mutator: Mutator = RandomMutator(cfg.grammar)

  /** synthesizer */
  val synthesizer: Synthesizer = RandomSynthesizer(cfg.grammar)

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  // current program pool
  private var pool: Vector[String] = Vector()

  // current iteration count
  private var iter: Int = 0

  // current id
  private var idCounter: Long = 0
  private def nextId: Long = { val id = idCounter; idCounter += 1; id }

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
