package esmeta.wji.lang

import java.io.File
import java.nio.file.Path

/** Locates spec files and provides a unified algorithm loader for the full
  * pipeline.
  */
object SpecFile:

  /** path to the WebAssembly JS API specification source */
  lazy val jsApiIndex: Path = locate("spectec/document/js-api/index.bs")

  /** path to the Web IDL specification source */
  lazy val webidlIndex: Path = locate("webidl/index.bs")

  /** algorithms extracted from [[webidlIndex]] must have one of these names */
  val webidlFilter: Set[String] = Set(
    "resolve",
    "react",
    "reject",
    "a new promise",
  )

  /** loads algorithms from all specs: all of jsApi plus the filtered subset of
    * webidl, merged into a single list for the shared pipeline
    */
  def loadAllAlgorithms(): List[Algorithm] =
    AlgorithmExtractor.extractFromFile(jsApiIndex) ++
    AlgorithmExtractor
      .extractFromFile(webidlIndex)
      .filter(a => a.name.exists(webidlFilter.contains))

  /** finds `relPath` in the nearest ancestor of the current directory */
  def locate(relPath: String): Path =
    val cwd = new File(".").getCanonicalFile
    Iterator
      .iterate(cwd)(_.getParentFile)
      .takeWhile(_ != null)
      .map(dir => new File(dir, relPath))
      .find(_.isFile)
      .getOrElse(
        throw new java.io.FileNotFoundException(
          s"could not locate $relPath above $cwd",
        ),
      )
      .toPath
