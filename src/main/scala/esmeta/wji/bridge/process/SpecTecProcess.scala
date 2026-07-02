package esmeta.wji.bridge.process

import esmeta.*

/** Manages the lifecycle of the SpecTec OCaml process, which exposes the
  * SpecTec Wasm interpreter over JSON-RPC on stdin/stdout.
  */
final class SpecTecProcess private (process: Process):
  def stdin = process.getOutputStream
  def stdout = process.getInputStream

  def close(): Unit = process.destroy()

object SpecTecProcess:
  val defaultExecutable: String = s"$SPECTEC_DIR/spectec/spectec"

  /** Directory holding the WJI/Wasm `.spectec` specification SpecTec loads. */
  val specDir: String = s"$SPECTEC_DIR/specification/wasm-latest"

  def start(): SpecTecProcess =
    // ProcessBuilder does not expand globs, so enumerate the `.spectec` files
    // ourselves and pass them as individual arguments. Lexicographic order
    // matches the files' numeric prefixes (0.* .. 7.*, X.*).
    val specFiles =
      Option(
        new java.io.File(specDir).listFiles((_, name) =>
          name.endsWith(".spectec"),
        ),
      )
        .getOrElse(Array.empty[java.io.File])
        .map(_.getPath)
        .sorted
        .toList
    start(defaultExecutable, (specFiles :+ "--server")*)

  def start(executable: String, args: String*): SpecTecProcess =
    val pb = new ProcessBuilder((executable +: args)*)
    pb.redirectError(ProcessBuilder.Redirect.INHERIT)
    new SpecTecProcess(pb.start())
