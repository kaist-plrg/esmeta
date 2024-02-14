package esmeta.es.util.deltadebugger

import esmeta.ESREDUCER_LOG_DIR
import esmeta.es.Ast
import esmeta.js.Target
import esmeta.interpreter.Interpreter
import esmeta.state.State
import esmeta.LINE_SEP
import esmeta.es.util.USE_STRICT
import esmeta.js.JSTrans
import esmeta.js.JSEngine
import esmeta.util.SystemUtils.*
import esmeta.es.util.injector.*
import esmeta.error.*
import esmeta.cfg.CFG
import scala.collection.mutable.{Queue as MutQueue}
import esmeta.es.Syntactic
import esmeta.es.Lexical
import scala.collection.mutable.{ArrayBuffer as MutVec}

trait DeltaDebugger[T]:
  def current: T
  def tryReduce(): Option[T]

type Script = String

class ESReducer(val initial: Script, val target: Target, val cfg: CFG)
  extends DeltaDebugger[Script]:

  private lazy val ast = cfg.scriptParser.from(initial)

  private val InterpreterTimeLimit: Int = 1

  private def getAssertion(candidate: Script): Script =
    val initSt: State = cfg.init.from(candidate)
    val exitSt: State =
      new Interpreter(
        initSt,
        timeLimit = Some(InterpreterTimeLimit),
        keepProvenance = true,
      ).result
    val injector: Injector = new Injector(initSt, exitSt, true, false)
    val sb = new StringBuilder()
    sb.append("// [EXIT] ")
    sb.append(injector.exitTag)
    sb.append(LINE_SEP)
    injector.assertions.foreach(assertion => {
      sb.append(assertion)
      sb.append(LINE_SEP)
    })
    sb.toString

  private def getActualTest(candidate: Script): Script =
    val strictScript = ESReducer.getStrictScript(candidate)
    val code = {
      if target.isTrans then
        val tempDir = s"$ESREDUCER_LOG_DIR/transpile-temp"
        val inDir = s"$tempDir/in"
        cleanDir(inDir)
        val scriptPath = s"$inDir/${target.name}.js"
        dumpFile(strictScript, scriptPath)
        val outDir = s"$tempDir/out"
        cleanDir(outDir)
        JSTrans.transpileDirUsingBinary(target.cmd, inDir, outDir).get
        val outPath = s"$outDir/${target.name}.js"
        readFile(outPath)
      else strictScript
    }
    val assertion = getAssertion(candidate)
    val isNormal = assertion.split(LINE_SEP).head.contains("[EXIT] normal")

    val libHead = "(()=>{"
    val libTail = "})();"
    val delayHead = "$delay(() => {"
    val delayTail = "});"
    if isNormal then
      List(
        code,
        libHead,
        Injector.assertionLib,
        delayHead,
        assertion,
        delayTail,
        libTail,
      ).mkString(LINE_SEP)
    else assertion + code

  private def isFailureInducing(candidate: Script): Boolean =
    val actualTest = getActualTest(candidate)

    val exitTagPattern = "(?<=\\[EXIT\\] )[^\n\r]*".r
    val exitTagRaw = exitTagPattern.findFirstIn(actualTest).get
    val exitTag = ExitTag(exitTagRaw).get

    val (concreteExitTag, stdout) =
      if (target.isTrans) {
        JSEngine
          .run(actualTest)
          .map((NormalTag, _))
          .recover(ESReducer.engineErrorResolver _)
          .get
      } else {
        JSEngine
          .runUsingBinary(target.cmd, actualTest)
          .map((NormalTag, _))
          .recover(ESReducer.engineErrorResolver _)
          .get
      }

    val sameExitTag = exitTag.equivalent(concreteExitTag)
    val pass = sameExitTag && stdout.isEmpty
    !pass

  private class MutAst(
    var ast: Ast,
    var parentalLink: Option[(MutAst, Int)],
    var children: MutVec[Option[MutAst]] = MutVec(),
  ) {
    def anyPostorder[R](
      f: MutAst => Option[R],
    ): Option[R] =
      this.ast match
        case Lexical(_, _) => None
        case Syntactic(_, _, _, children) =>
          this.children.foldLeft[Option[R]](None) { (curr, child) =>
            curr match
              case Some(_) => curr
              case None =>
                child match
                  case None        => None
                  case Some(child) => child.anyPostorder(f)
          } match {
            case result @ Some(_) => result
            case None =>
              Range(this.children.length, children.length)
                .foldLeft[Option[R]](None) { (curr, i) =>
                  curr match
                    case Some(_) => curr
                    case None =>
                      val child =
                        children(i).map(ast => MutAst(ast, Some(this, i)))
                      this.children += child
                      child match {
                        case None        => None
                        case Some(child) => child.anyPostorder(f)
                      }
                }
          } match {
            case result @ Some(_) => result
            case None =>
              f(this)
          }
  }

  private var script: Script = initial
  private var finished = false

  private var mutAst: Option[MutAst] = None // if None, haven't tried to reduce
  private var succeedingQueue: MutQueue[MutAst] = MutQueue()
  private var nextSucceedingQueue: MutQueue[MutAst] = MutQueue()

  private class Result
  private case class Success(prunedScript: Script) extends Result
  private case object Failure extends Result
  private case object Impossible extends Result

  private def trySucceedingOnce(): Result = {
    if succeedingQueue.isEmpty then
      if nextSucceedingQueue.isEmpty then Some(Impossible)
      else
        succeedingQueue = nextSucceedingQueue
        nextSucceedingQueue = MutQueue()
        None
    else None
  } match
    case Some(result) => result
    case None =>
      val succeedee = succeedingQueue.dequeue()
      succeedee.ast match {
        case Syntactic(name, args, _, _) => {
          val succession = succeedee.anyPostorder { node =>
            if node eq succeedee then None
            else
              node.ast match
                case Syntactic(`name`, `args`, _, _) =>
                  val updateQueue: MutQueue[(MutAst, Ast)] = MutQueue()
                  var ast = node.ast
                  var cursor = succeedee
                  updateQueue.enqueue((cursor, ast))
                  while (cursor.parentalLink.isDefined) {
                    val (parent, idx) = cursor.parentalLink.get
                    val Syntactic(name, args, rhsIdx, children) = parent.ast
                    val newChildren = Range(0, children.length).map { i =>
                      if i == idx then Some(ast)
                      else children(i)
                    }.toVector
                    val newAst = Syntactic(name, args, rhsIdx, newChildren)
                    updateQueue.enqueue((parent, newAst))
                    ast = newAst
                    cursor = parent
                  }
                  if ast.valid(cfg.grammar) then
                    val prunedScript: Script = ast.toString(cfg.grammar)
                    // println(s"  .         TRIED: ${prunedScript}")
                    if script != prunedScript then
                      val conformFailure =
                        try {
                          isFailureInducing(prunedScript)
                        } catch {
                          case e: NotSupported => false
                          case e               => throw e
                        }
                      if conformFailure then
                        updateQueue.foreach { (mutAst, ast) =>
                          mutAst.ast = ast
                        }
                        succeedee.children = node.children
                        Some(prunedScript)
                      // println(s"  .       SUCCESS: ${prunedScript}")
                      else None
                    else None
                  else None
                case _ => None
          }
          nextSucceedingQueue.enqueueAll(succeedee.children.flatten)
          succession match {
            case Some(result) => Success(result)
            case None         => Failure
          }
        }
        case Lexical(_, _) => Failure
      }

  private def uncheckedTryReduce(): Option[Script] =
    if mutAst.isEmpty then
      val mutAstInner = MutAst(ast, None, MutVec())
      mutAst = Some(mutAstInner)
      succeedingQueue.enqueue(mutAstInner)
    var result: Option[Script] = None
    while ({
      trySucceedingOnce() match {
        case Failure => true
        case Success(prunedScript) =>
          result = Some(prunedScript)
          false
        case Impossible => false
      }
    }) {}
    result

  def current: Script = script

  def tryReduce(): Option[Script] =
    if finished then None
    else
      uncheckedTryReduce() match
        case None =>
          finished = true
          None
        case Some(reduced) =>
          script = reduced
          Some(reduced)

object ESReducer {
  def getStrictScript(candidate: Script): Script =
    USE_STRICT + candidate + LINE_SEP

  def engineErrorResolver(engineError: Throwable): (ExitTag, String) =
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

  val errorPattern = "[\\w]*Error(?=: )".r
}
