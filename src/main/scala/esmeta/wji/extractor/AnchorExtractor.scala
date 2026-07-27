package esmeta.wji.extractor

import esmeta.wji.lang.*
import esmeta.wji.spec.SpecPatch

import java.nio.file.{Files, Path}

/** Extracts structured data out of js-api/index.bs's own `<pre
  * class="anchors">` block (spectec/document/js-api/index.bs) — the Bikeshed
  * external-link registry declaring every term this file references but doesn't
  * itself define (which spec it's from, what kind of thing it is, and the URL
  * it resolves to).
  *
  * Lets WJI's own hand-maintained tables (e.g. `WasmHost.names`) be
  * cross-checked against what the spec source actually declares, rather than
  * trusting a hand-copied list to stay in sync as the spec evolves — see
  * `AnchorExtractorSpec`.
  */
object AnchorExtractor:

  private val PreOpen = """<pre\s+class=['"]anchors['"]>""".r
  private val PreClose = "</pre>"

  /** Every key `key: value` pair on a line, split on top-level `;` (the block
    * writes multiple assignments on one line this way, e.g. `type: dfn; for:
    * ECMAScript`).
    */
  private val KeyValue = """([a-zA-Z]+):\s*(.*)""".r

  /** The `key:`/`value` context accumulated from every line at or above a given
    * indentation — everything but `urlPrefix` is simple "most specific wins"
    * overriding; `urlPrefix` instead *concatenates* onto whatever was already
    * accumulated when the new value doesn't look like a fresh absolute URL (no
    * `://`) — mirroring how the source itself nests a relative `urlPrefix:
    * valid/matching.html` inside an outer absolute one to extend it, rather
    * than replace it.
    */
  private case class Context(
    urlPrefix: String = "",
    spec: Option[String] = None,
    typ: Option[String] = None,
    forScope: Option[String] = None,
    url: Option[String] = None,
  ):
    def merge(kvs: Map[String, String]): Context =
      Context(
        urlPrefix = kvs.get("urlPrefix") match
          case Some(p) if p.contains("://") => p
          case Some(p)                      => urlPrefix + p
          case None                         => urlPrefix
        ,
        spec = kvs.get("spec").orElse(spec),
        typ = kvs.get("type").orElse(typ),
        forScope = kvs.get("for").orElse(forScope),
        url = kvs.get("url").orElse(url),
      )

  /** Parses every anchor entry out of the `<pre class="anchors">...</pre>`
    * block in `source` (js-api/index.bs's raw text; entries outside that block
    * are ignored).
    *
    * The block is an indentation-nested `key: value[; key: value]*` format: a
    * key set on one line applies to every more-deeply-indented line below it,
    * until a sibling or shallower line overrides it — the same nesting idiom
    * `AlgorithmExtractor.parseBody` reconstructs `<div algorithm>` step
    * structure with, via the same indentation-stack technique.
    */
  def extract(source: String): List[Anchor] =
    val block = extractBlock(source)
    val results = collection.mutable.ListBuffer[Anchor]()
    // stack of (indent, context) — context(es) active at/above that indent;
    // seeded with a sentinel below any real indentation level
    val stack = collection.mutable.ArrayBuffer[(Int, Context)]((-1, Context()))

    for rawLine <- block.linesIterator do
      if rawLine.trim.nonEmpty then
        val indent = rawLine.takeWhile(_ == ' ').length
        while stack.length > 1 && stack.last._1 >= indent do
          stack.remove(stack.length - 1)

        val kvs = rawLine.trim
          .split(";")
          .map(_.trim)
          .collect { case KeyValue(k, v) => k -> v.trim }
          .toMap
        val context = stack.last._2.merge(kvs)

        kvs.get("text").foreach { text =>
          context.url.foreach { url =>
            results += Anchor(
              text,
              context.urlPrefix + url,
              context.spec,
              context.typ,
              context.forScope,
            )
          }
        }

        stack += (indent -> context)

    results.toList

  def extractFromFile(path: Path): List[Anchor] =
    extract(SpecPatch(Files.readString(path)))

  private def extractBlock(source: String): String =
    PreOpen.findFirstMatchIn(source) match
      case None => ""
      case Some(m) =>
        val end = source.indexOf(PreClose, m.end)
        if end < 0 then source.substring(m.end)
        else source.substring(m.end, end)

  /** Every anchor, among already-extracted `anchors`, whose URL points at the
    * Wasm Core Spec's Embedding interface (`appendix/embedding.html#embed-...`)
    * — i.e. every name `esmeta.wji.bridge.host.WasmHost` should mirror (see
    * `AnchorExtractorSpec`).
    *
    * `error`/`exception` are excluded: both alias the same `#embed-error`
    * anchor, the shared "operation failed" result marker every embedding
    * function's own signature can return (`... : X | error`), not a callable
    * function of their own.
    */
  def embeddingFunctionNames(anchors: List[Anchor]): Set[String] =
    anchors
      .filter(_.url.contains("appendix/embedding.html#embed-"))
      .map(_.text)
      .filterNot(name => name == "error" || name == "exception")
      .toSet
