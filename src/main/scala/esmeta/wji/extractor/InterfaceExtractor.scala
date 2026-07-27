package esmeta.wji.extractor

import esmeta.wji.lang.*
import esmeta.wji.spec.SpecPatch

import esmeta.util.HtmlUtils.*
import java.nio.file.{Files, Path}

/** Extracts [[Interface]]s out of `<pre class="idl">` blocks in the Bikeshed
  * source of the WebAssembly JS API specification
  * (`spectec/document/js-api/index.bs`).
  *
  * Only `interface Name { ... };` declarations are extracted — a `<pre
  * class="idl">` block also commonly holds `dictionary`/`enum` declarations
  * (e.g. `WebAssemblyCompileOptions` right next to `WebAssembly`), which are a
  * different WebIDL construct and matched separately elsewhere (see
  * `ExprParser`'s `{{Dict/member}}` handling).
  */
object InterfaceExtractor:

  /** matches a `<pre class="idl">...</pre>` block's open tag — there are
    * several of these scattered through the file (one per section), each
    * possibly holding a mix of `dictionary`/`enum`/`interface` declarations
    */
  private val IdlPreOpen = """<pre\s+class=['"]idl['"]>""".r
  private val IdlPreClose = "</pre>"

  /** matches the start of an `interface Name {` declaration; the body end is
    * found separately by [[findBodyEnd]] (a member can itself contain a nested
    * `{}`, e.g. `optional WebAssemblyCompileOptions options = {}`, so a regex
    * spanning the whole body can't safely find the matching close brace)
    */
  private val InterfaceOpen = """\binterface\s+([A-Za-z][A-Za-z0-9]*)\s*\{""".r

  def extract(source: String): List[Interface] =
    idlBlocks(source).flatMap(extractFromBlock)

  private def idlBlocks(source: String): List[String] =
    IdlPreOpen
      .findAllMatchIn(source)
      .toList
      .flatMap { m =>
        val end = source.indexOf(IdlPreClose, m.end)
        if end < 0 then None else Some(source.substring(m.end, end))
      }

  private def extractFromBlock(block: String): List[Interface] =
    InterfaceOpen
      .findAllMatchIn(block)
      .toList
      .flatMap { m =>
        val name = m.group(1)
        for bodyEnd <- findBodyEnd(block, m.end) yield
          val body = block.substring(m.end, bodyEnd)
          Interface(name, splitMembers(body))
      }

  def extractFromFile(path: Path): List[Interface] =
    extract(SpecPatch(Files.readString(path)))

  /** finds the index of the `}` matching the `{` whose body starts at
    * `bodyStart`, accounting for any `{...}` nested inside (e.g. a member's `=
    * {}` default value) via simple brace-depth counting
    */
  private def findBodyEnd(source: String, bodyStart: Int): Option[Int] =
    var depth = 1
    var i = bodyStart
    while i < source.length do
      source(i) match
        case '{' => depth += 1
        case '}' =>
          depth -= 1
          if depth == 0 then return Some(i)
        case _ =>
      i += 1
    None

  /** splits an interface body into its member declarations. Unescaped first —
    * generic types are written HTML-escaped (`sequence&lt;X&gt;`), and
    * `&lt;`/`&gt;` themselves end in `;`, which a raw split would wrongly treat
    * as a member separator. Safe to split on a bare `;` after that: no member
    * in this corpus nests one inside its own `{}`/`()` (the only nested braces
    * are empty `= {}` default values).
    */
  private def splitMembers(body: String): List[String] =
    body.unescapeHtml.split(";").map(_.trim).filter(_.nonEmpty).toList
