package esmeta.util

import esmeta.LINE_SEP
import esmeta.util.BaseUtils.{warn, error}
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

  /** default engine: (command, version) */
  lazy val defaultEngine: Option[(String, String)] = Try {
    // d8
    "d8 -e ''".!!
    (
      "d8 -e",
      runUsingBinaryAndGetStdout("d8 -e", "console.log(version());").get,
    )
  }.recoverWith(_ =>
    Try {
      // node
      "node -e ''".!!
      nodeWarning
      ("node -e", runUsingBinaryAndGetStdout("node", "--version").get)
    },
  ).recoverWith(e =>
    validityCheckerWarning
    // GraalVM
    if (useGraal)
      Using(Engine.create()) { engine =>
        ("GraalVM", engine.getVersion())
      }
    else
      Failure(e),
  ).recoverWith(_ =>
    Try {
      // js
      "js -e ''".!!
      ("js -e", runUsingBinaryAndGetStdout("js", "--version").get)
    },
  ).toOption

  lazy val defaultEngineToString = defaultEngine
    .map {
      case (e, v) => s"Engine: $e${LINE_SEP}Version: $v"
    }
    .getOrElse("NO-DEFAULT-ENGINE")

  /** warn user if node is used */
  private lazy val nodeWarning = warn(
    "Node is used as the default engine. This may degrade the speed of JSEngine.",
  )

  /** warn user if d8 or node can't be used */
  private lazy val validityCheckerWarning = warn(
    "Could not use d8 or node. Validity checker might pass the invalid program.",
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
          warn("Graal is running on interpreted mode.")
          throw Error()
        else
          try {
            context.eval("js", "")
          } catch
            case e => {
              warn("Unable to run js using Graal. try `gu --jvm install js`.")
              throw e
            }
      }.isSuccess
    catch {
      case e: Error =>
        warn("Unable to run Graal.")
        false
    }

  /** run a javascript code with default engine */
  def run(src: String, timeout: Option[Int] = None): Try[Unit] = defaultEngine
    .map {
      case ("GraalVM", _) => runUsingGraal(src, timeout)
      case (e, _)         => runUsingBinary(e, src) // TODO: implement timeout
    }
    .getOrElse(
      error("Default engine is not set"),
    )

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
  def runUsingContext(
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

  def runUsingGraal(src: String, timeout: Option[Int] = None): Try[Unit] =
    usingContext((context, _) => runUsingContext(src, context, timeout))

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
