package esmeta.js

import esmeta.*
import scala.sys.process._
import java.util.concurrent.LinkedBlockingQueue
import java.io._

case class REPL(file: String) {

  /** asynchronous input and output for repl binary */
  private val inputString = new LinkedBlockingQueue[String](1)
  private val outputString = new LinkedBlockingQueue[String](1)

  /** process that runs repl in background */
  private val procIO = new ProcessIO(inputFn, outputFn, errorFn, true)
  private val process = file.run(procIO)

  /** The standard input passing function */
  private[this] def inputFn(stdin: OutputStream): Unit =
    val writer = new BufferedWriter(new OutputStreamWriter(stdin))
    while (true) {
      val input = inputString.take /* Blocked */
      if (input == "CLOSE") then stdin.close()
      else
        writer.write(input + "\n")
        writer.flush()
    }

  /** The standard output obtaining function */
  private[this] def outputFn(stdOut: InputStream): Unit =
    val reader = new BufferedReader(new InputStreamReader(stdOut))
    val buffer: StringBuilder = new StringBuilder()

    while (true) {
      val line = reader.readLine() /* Blocked */

      if line == null then
        outputString.put("PROCESS TERMINATED")
        return
      else if line == "DONE" then
        outputString.put(buffer.toString)
        buffer.setLength(0)
      else buffer.append(line + "\n")
    }

  /** The standard error obtaining function */
  private[this] def errorFn(stdErr: InputStream): Unit =
    val reader = new BufferedReader(new InputStreamReader(stdErr))

    while (true) {
      val line = reader.readLine() /* Blocked */
      if line == null then return
    }

  /** Give a string input to the repl and gets its output */
  def enter(input: String): String =
    if (process.isAlive) {
      inputString.put(input)
      /* REPL will run asynchronously here */
      outputString.take.trim /* Blocked */
    } else {
      throw new esmeta.error.ESMetaError(
        "REPL is terminated, which is unexpected",
      )
    }

  def close: Unit = inputString.put("CLOSE")
}
