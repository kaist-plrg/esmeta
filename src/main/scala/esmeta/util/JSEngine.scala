package esmeta.util

import scala.util._
import scala.collection.mutable.{Map => MMap}
import org.graalvm.polyglot.*
import java.io.*
import java.time.Duration.ZERO
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import java.util.concurrent.TimeoutException

/** JavaScript Engine utilities */
object JSEngine {

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
    if (!useGraal) ???
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
    if (!useGraal) ???
    val stat = Status()
    out.reset
    timeout.foreach(millis => registerTimeout(context, millis, stat))
    stat.running = true
    try {
      context.eval("js", src)
      out.toString
    } finally stat.done = true

  def usingContext[T](f: (Context, ByteArrayOutputStream) => T): Try[T] =
    if (!useGraal) ???
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
    if (!useGraal) ???

    val (context, out) = getContext(id)

    Try(context.eval("js", src))

  /** execute a javascript program with stored context, and gets its stdout */
  def runInContextAndGetStdout(id: String, src: String): Try[String] =
    if (!useGraal) ???

    val (context, out) = getContext(id)

    Try(context.eval("js", src)).map(_ => {
      val result = out.toString
      out.flush()
      result
    })
}
