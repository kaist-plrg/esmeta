package esmeta.util

import scala.util.{Using, Try}
import org.graalvm.polyglot.*
import java.io.ByteArrayOutputStream

/** JavaScript Engine utilities */
object JSEngine {

  /** Check if Graal can be used in this environment */
  lazy val useGraal: Boolean = Using(
    Context
      .newBuilder("js")
      .option("engine.WarnInterpreterOnly", "false")
      .build(),
  ) { context =>
    if (context.getEngine.getImplementationName == "Interpreted") then
      println("[Warning] sbt is not running on Graal.")
      throw Error()
    else
      try {
        context.eval("js", "")
      } catch
        case e => {
          println(
            "[Warning] Unable to run js using Graal. try `gu install js`.",
          )
          throw e
        }
  }.isSuccess

  /** execute a javascript program */
  def run(src: String): Try[Unit] =
    if (!useGraal)
      // TODO: use shell to run js code
      ???

    Using(Context.create("js")) { context =>
      context.eval("js", src)
    }

  /** execute a javascript program, and extract a value of variable */
  def runAndGetVar(src: String, x: String): Try[String] =
    if (!useGraal)
      // TODO: use shell to run js code
      ???

    Using(Context.create("js")) { context =>
      context.eval("js", src)
      context.getBindings("js").getMember(x).asString()
    }

  /** execute a javascript program, and gets is stdout */
  def runAndGetStdout(src: String): Try[String] =
    if (!useGraal)
      // TODO: use shell to run js code
      ???

    val out = new ByteArrayOutputStream()
    Using(Context.newBuilder("js").out(out).build()) { context =>
      context.eval("js", src)
      out.toString
    }
}
