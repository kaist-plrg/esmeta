package esmeta.util

import scala.util.{Using, Try}
import org.graalvm.polyglot.*
import java.io.ByteArrayOutputStream

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

  /** execute a javascript program */
  def run(src: String): Try[Unit] =
    if (!useGraal) ???

    Using(Context.create("js")) { context =>
      context.eval("js", src)
    }

  /** execute a javascript program, and extract a value of variable */
  def runAndGetVar(src: String, x: String): Try[String] =
    if (!useGraal) ???

    Using(Context.create("js")) { context =>
      context.eval("js", src)
      context.getBindings("js").getMember(x).asString()
    }

  /** execute a javascript program, and gets its stdout */
  def runAndGetStdout(src: String): Try[String] =
    if (!useGraal) ???

    val out = new ByteArrayOutputStream()
    Using(Context.newBuilder("js").out(out).build()) { context =>
      context.eval("js", src)
      out.toString
    }

  def runAndGetStdout(srcs: List[String]): Try[List[String]] =
    if (!useGraal) ???

    val out = new ByteArrayOutputStream()
    Using(Context.newBuilder("js").out(out).build()) { context =>
      srcs.map(src =>
        context.eval("js", src)
        val result = out.toString
        out.reset
        result,
      )
    }
}
