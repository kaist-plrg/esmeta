package esmeta.wji.spec

import java.io.File
import java.nio.file.Path

/** Locates spec files, used by [[AlgorithmExtractor]]/[[DefinitionExtractor]]/
  * [[AnchorExtractor]] and the unified [[esmeta.wji.extractor.Extractor]].
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
    "get a copy of the buffer source",
    "internally create a new object implementing the interface",
    "create an interface prototype object",
    "create an interface object",
    "define the regular attributes",
    "define the attributes",
    "attribute getter",
    "attribute setter",
    "define the regular operations",
    "define the operations",
    "creating an operation function",
    "define the iteration methods",
    "define the asynchronous iteration methods",
    "define the unforgeable regular operations",
    "define the unforgeable regular attributes",
    "define the static attributes",
    "define the static operations",
    "define the constants",
  )

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
