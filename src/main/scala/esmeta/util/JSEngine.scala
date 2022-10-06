package esmeta.util

import esmeta.LINE_SEP
import esmeta.error.{NoGraal, TimeoutException}
import java.io.*
import java.time.Duration.ZERO
import org.graalvm.polyglot.*
import scala.collection.mutable.{Map => MMap}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util._
import sys.process._
import scala.language.postfixOps
import java.util.StringJoiner

/** JavaScript Engine utilities */
object JSEngine {

  /** default engine */
  // TODO: change this into d8
  lazy val defaultEngine: Try[String] = Try {
    "d8 -e ''".!!
    "d8 -e"
  }.recoverWith(_ =>
    Try {
      "node -e ''".!!
      "node -e"
    },
  )

  /** Check if Graal can be used in this environment */
  lazy val useGraal: Boolean =
    try
      Using(
        Context
          .newBuilder("js")
          .option("engine.WarnInterpreterOnly", "false")
          .build(),
      ) { context =>
        if (context.getEngine.getImplementationName == "Interpreted") then
          println("[Warning] Graal is running on interpreted mode.")
          throw Error()
        else
          try {
            context.eval("js", "")
          } catch
            case e => {
              println(
                "[Warning] Unable to run js using Graal. try `gu --jvm install js`.",
              )
              throw e
            }
      }.isSuccess
    catch {
      case e: Error =>
        println("[Warning] Unable to run Graal.")
        false
    }

  lazy val noDefaultWarning = println(
    "[Warning] Unable to use default engine. Trying to use graal instead..",
  )

  def runWithDefault(src: String): Try[Unit] = {
    defaultEngine.map(e => runUsingBinary(e, src)).getOrElse {
      noDefaultWarning
      runSingle(src)
    }
  }

  // -------------------------------------------------------------------------
  // runners in temporal context
  // -------------------------------------------------------------------------

  /** Exception that indicates given JS code throws exception */
  class JSException(message: String) extends Exception(message)

  /** resolve PolyglotException to other exceptions */
  def polyglotExceptionResolver[T](e: Throwable): Try[T] = e match {
    case e: PolyglotException if (e.isInterrupted || e.isCancelled) =>
      Failure(TimeoutException("JSEngine timeout"))
    case e: PolyglotException if e.isGuestException =>
      Failure(JSException(e.getMessage))
    case e: PolyglotException if e.isHostException =>
      Failure(e.asHostException)
    case _ =>
      Failure(e)
  }

  case class Status(var running: Boolean = false, var done: Boolean = false)

  /** register timeout to context in milliseconds */
  def registerTimeout(context: Context, timeout: Int, stat: Status) =
    Future {
      while (!stat.done) {
        Thread.sleep(timeout)
        if (!stat.done && stat.running)
          // TODO race condition:
          // new eval is performed between
          // (!stat.done) check and interrupt
          context.interrupt(ZERO)
      }
    }

  /** execute a javascript program */
  def run(
    src: String,
    context: Context,
    timeout: Option[Int] = None,
  ): Unit =
    if (!useGraal) throw NoGraal
    val stat = Status()
    timeout.foreach(millis => registerTimeout(context, millis, stat))
    stat.running = true
    try context.eval("js", src)
    finally stat.done = true

  /** execute a javascript program, and gets its stdout */
  def runAndGetStdout(
    src: String,
    context: Context,
    out: ByteArrayOutputStream,
    timeout: Option[Int] = None,
  ): String =
    if (!useGraal) throw NoGraal
    val stat = Status()
    out.reset
    timeout.foreach(millis => registerTimeout(context, millis, stat))
    stat.running = true
    try {
      context.eval("js", src)
      out.toString
    } finally stat.done = true

  def usingContext[T](f: (Context, ByteArrayOutputStream) => T): Try[T] =
    if (!useGraal) throw NoGraal
    val out = new ByteArrayOutputStream()
    Using(Context.newBuilder("js").out(out).build()) { context =>
      f(context, out)
    }.recoverWith(e => polyglotExceptionResolver(e))

  def runSingle(src: String, timeout: Option[Int] = None): Try[Unit] =
    usingContext((context, _) => run(src, context, timeout))

  // -------------------------------------------------------------------------
  // runners in saved context
  // -------------------------------------------------------------------------

  /** Saved contexts */
  val contextMap: MMap[String, (Context, ByteArrayOutputStream)] = MMap()

  def getContext(id: String): (Context, ByteArrayOutputStream) =
    contextMap.getOrElseUpdate(
      id, {
        val out = new ByteArrayOutputStream()
        (Context.newBuilder("js").out(out).build(), out)
      },
    )

  /** execute a javascript program with the stored context */
  def runInContext(id: String, src: String): Try[Value] =
    if (!useGraal) throw NoGraal

    val (context, out) = getContext(id)

    Try(context.eval("js", src))

  /** execute a javascript program with stored context, and gets its stdout */
  def runInContextAndGetStdout(id: String, src: String): Try[String] =
    if (!useGraal) throw NoGraal

    val (context, out) = getContext(id)

    Try(context.eval("js", src)).map(_ => {
      val result = out.toString
      out.flush()
      result
    })

  // -------------------------------------------------------------------------
  // runners by executing shell command with path to binary
  // -------------------------------------------------------------------------
  def runUsingBinary(runner: String, src: String): Try[Unit] = Try {
    val escapedSrc = escape(src)
    val stderr = new StringJoiner(LINE_SEP)
    s"$runner $escapedSrc" ! ProcessLogger(
      out => (),
      err => stderr.add(err),
    ) match {
      case 0  =>
      case st => throw new Exception(stderr.toString),
    }
  }

  def runUsingBinaryAndGetStdout(runner: String, src: String): Try[String] =
    Try {
      val escapedSrc = escape(src)
      val stdout = new StringJoiner(LINE_SEP)
      val stderr = new StringJoiner(LINE_SEP)
      s"$runner $escapedSrc" ! ProcessLogger(
        out => stdout.add(out),
        err => stderr.add(err),
      ) match {
        case 0  => stdout.toString
        case st => throw new Exception(stderr.toString),
      }
    }

  /** escape a string to a shell-safe string, enclosed by single quote */
  private def escape(string: String): String =
    val replaced = string.replace("'", "'\"'\"'") // replace I'm to I'"'"'m
    s"'$replaced'"
}
