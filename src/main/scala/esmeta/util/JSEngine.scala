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

  val TIMEOUT = 1000 // default 1000ms

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

  /** register timeout to context in milliseconds */
  def registerTimeout(context: Context, timeout: Int) =
    Future {
      Thread.sleep(timeout)
      context.interrupt(ZERO)
      context.close(cancelIfExecuting = true)
    }

  /** execute a javascript program */
  def run(src: String, timeout: Int = TIMEOUT): Try[Unit] =
    if (!useGraal) ???

    Using(Context.create("js")) { context =>
      registerTimeout(context, timeout)
      context.eval("js", src)
      ()
    }.recoverWith(e => polyglotExceptionResolver(e))

  /** execute a javascript program, and gets its stdout */
  def runAndGetStdout(src: String, timeout: Int = TIMEOUT): Try[String] =
    runAndGetStdouts(List(src), timeout).map(_.head)

  def runAndGetStdouts(
    srcs: List[String],
    timeout: Int = TIMEOUT,
  ): Try[List[String]] =
    if (!useGraal) ???

    val out = new ByteArrayOutputStream()
    Using(Context.newBuilder("js").out(out).build()) { context =>
      registerTimeout(context, timeout)
      srcs.map(src =>
        context.eval("js", src)
        val result = out.toString
        out.reset
        result,
      )
    }.recoverWith(e => polyglotExceptionResolver(e))

  /** Created contexts */
  val contextMap: MMap[String, (Context, OutputStream)] = MMap()

  def getContext(id: String): (Context, OutputStream) =
    contextMap.getOrElseUpdate(
      id, {
        val out = new ByteArrayOutputStream()
        (Context.newBuilder("js").out(out).build(), out)
      },
    )

  /** execute a javascript program with the given context */
  def runInContext(id: String, src: String): Try[Value] =
    if (!useGraal) ???

    val (context, out) = getContext(id)

    Try(context.eval("js", src))

  /** execute a javascript program with context, and gets its stdout */
  def runInContextAndGetStdout(id: String, src: String): Try[String] =
    if (!useGraal) ???

    val (context, out) = getContext(id)

    Try(context.eval("js", src)).map(_ => {
      val result = out.toString
      out.flush()
      result
    })
}
