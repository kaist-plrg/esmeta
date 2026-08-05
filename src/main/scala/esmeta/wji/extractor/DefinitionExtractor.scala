package esmeta.wji.extractor

import esmeta.wji.lang.*
import esmeta.wji.spec.SpecPatch

import esmeta.util.HtmlUtils.*
import java.nio.file.{Files, Path}

/** Extracts [[Definition]]s (kind [[DefinitionKind.Namespace]] or
  * [[DefinitionKind.Operation]]) out of `<pre class="idl">` blocks in the
  * Bikeshed source of the WebAssembly JS API specification
  * (`spectec/document/js-api/index.bs`) — a `namespace WebAssembly { ... }`
  * declaration and `interface Name { ... }` declarations (e.g. `Module`,
  * `Instance`, `Memory`, `Table`, `Global`, `Tag`, `Exception`), each
  * optionally preceded by a `[...]` extended attribute list (e.g.
  * `[LegacyNamespace=WebAssembly, Exposed=*]`).
  *
  * Only `namespace`/`interface` blocks are extracted — a `<pre class="idl">`
  * block also commonly holds `dictionary`/`enum` declarations (e.g.
  * `WebAssemblyCompileOptions` right next to `WebAssembly`), which are a
  * different WebIDL construct and matched separately elsewhere (see
  * `ExprParser`'s `{{Dict/member}}` handling).
  */
object DefinitionExtractor:

  /** matches a `<pre class="idl">...</pre>` block's open tag — there are
    * several of these scattered through the file (one per section), each
    * possibly holding a mix of `dictionary`/`enum`/`interface`/`namespace`
    * declarations
    */
  private val IdlPreOpen = """<pre\s+class=['"]idl['"]>""".r
  private val IdlPreClose = "</pre>"

  /** matches the start of an `interface Name {` or `namespace Name {`
    * declaration; the body end is found separately by [[findBodyEnd]] (a member
    * can itself contain a nested `{}`, e.g. `optional WebAssemblyCompileOptions
    * options = {}`, so a regex spanning the whole body can't safely find the
    * matching close brace)
    */
  private val DefOpen =
    """\b(interface|namespace)\s+([A-Za-z][A-Za-z0-9]*)\s*\{""".r

  def extract(source: String): List[Definition] =
    idlBlocks(source).flatMap(extractFromBlock)

  def extractFromFile(path: Path): List[Definition] =
    extract(SpecPatch(Files.readString(path)))

  private def idlBlocks(source: String): List[String] =
    IdlPreOpen
      .findAllMatchIn(source)
      .toList
      .flatMap { m =>
        val end = source.indexOf(IdlPreClose, m.end)
        if end < 0 then None else Some(source.substring(m.end, end))
      }

  private def extractFromBlock(block: String): List[Definition] =
    DefOpen
      .findAllMatchIn(block)
      .toList
      .flatMap { m =>
        val keyword = m.group(1)
        val name = m.group(2)
        for bodyEnd <- findBodyEnd(block, m.end) yield
          val body = block.substring(m.end, bodyEnd)
          val members = splitMemberTexts(body).map(parseMember)
          val extAttr = extAttrBefore(block, m.start)
          val kind =
            if keyword == "namespace" then DefinitionKind.Namespace
            else DefinitionKind.Interface
          Definition(name, members, kind, extAttr)
      }

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

  /** splits an interface/namespace body into its member declarations' raw text.
    * Unescaped first — generic types are written HTML-escaped
    * (`sequence&lt;X&gt;`), and `&lt;`/`&gt;` themselves end in `;`, which a
    * raw split would wrongly treat as a member separator. Safe to split on a
    * bare `;` after that: no member in this corpus nests one inside its own
    * `{}`/`()` (the only nested braces are empty `= {}` default values).
    */
  private def splitMemberTexts(body: String): List[String] =
    body.unescapeHtml.split(";").map(_.trim).filter(_.nonEmpty).toList

  /** parses one member declaration's raw text (see [[splitMemberTexts]]) into a
    * [[Member]] — one of:
    *   - `constructor(PARAMS)` → an [[Operation]] with `id = "constructor"` and
    *     `kind = MemberKind.Constructor` (WebIDL constructors have no
    *     name/return type of their own)
    *   - `[readonly] attribute TYPE ID` → an [[Attribute]]
    *   - `[static] RETTYPE ID(PARAMS)` → an [[Operation]], `kind` reflecting
    *     whether `static` was present
    *
    * Any leading `[...]` extended attribute list (not seen on a member in this
    * corpus, but allowed by WebIDL in general) is captured either way.
    */
  private def parseMember(raw: String): Member =
    val (extAttr, rest) = stripLeadingExtAttr(raw)
    if rest.startsWith("constructor(") && rest.endsWith(")") then
      val params = parseParams(
        rest.substring("constructor(".length, rest.length - 1),
      )
      Operation("constructor", params, "", MemberKind.Constructor, extAttr)
    else if rest.startsWith("readonly attribute ") then
      val (ty, id) = splitTypeAndId(rest.stripPrefix("readonly attribute "))
      Attribute(id, ty, readonly = true, extAttr = extAttr)
    else if rest.startsWith("attribute ") then
      val (ty, id) = splitTypeAndId(rest.stripPrefix("attribute "))
      Attribute(id, ty, readonly = false, extAttr = extAttr)
    else
      val (head, kind) =
        if rest.startsWith("static ") then
          (rest.stripPrefix("static "), MemberKind.StaticOperation)
        else (rest, MemberKind.RegularOperation)
      val close = head.length - 1
      val open = findMatchingOpen(head, close)
      val (ret, id) = splitTypeAndId(head.substring(0, open).trim)
      val params = parseParams(head.substring(open + 1, close))
      Operation(id, params, ret, kind, extAttr)

  /** splits `RETTYPE ID` (an operation's return type + name) or `TYPE ID` (an
    * attribute's type + name) on its last top-level whitespace — the id is
    * always the final token, so this works even when the type itself is
    * multiple words (`unsigned long`) or a parenthesized union (`(DOMString or
    * undefined)`)
    */
  private def splitTypeAndId(head: String): (String, String) =
    val tokens = splitTopLevelWhitespace(head)
    (tokens.init.mkString(" "), tokens.last)

  /** parses a top-level-comma-separated WebIDL parameter list, e.g.
    * `[AllowResizable] AllowSharedBufferSource bytes, optional
    * WebAssemblyCompileOptions options = {}`
    */
  private def parseParams(raw: String): List[Param] =
    splitTopLevel(raw, ',').map(_.trim).filter(_.nonEmpty).map(parseParam)

  /** parses one `[ExtAttrs] optional? TYPE NAME (= DEFAULT)?` parameter — the
    * name itself isn't kept (see [[Param]]'s doc)
    */
  private def parseParam(raw: String): Param =
    val (extAttr, afterExtAttr) = stripLeadingExtAttr(raw)
    var tokens = splitTopLevelWhitespace(afterExtAttr)
    val optional = tokens.headOption.contains("optional")
    if optional then tokens = tokens.tail
    val eqIdx = tokens.indexOf("=")
    val (typeAndName, default) =
      if eqIdx < 0 then (tokens, "")
      else (tokens.take(eqIdx), tokens.drop(eqIdx + 1).mkString(" "))
    val ty = typeAndName.init.mkString(" ")
    Param(ty, optional, default, extAttr)

  /** strips a leading `[...]` extended attribute list, if present, returning
    * the parsed attributes and the remaining trimmed text
    */
  private def stripLeadingExtAttr(
    raw: String,
  ): (List[ExtendedAttribute], String) =
    val trimmed = raw.trim
    if !trimmed.startsWith("[") then (Nil, trimmed)
    else
      var depth = 0
      var i = 0
      var closeIdx = -1
      while i < trimmed.length && closeIdx < 0 do
        trimmed(i) match
          case '[' => depth += 1
          case ']' =>
            depth -= 1
            if depth == 0 then closeIdx = i
          case _ =>
        i += 1
      if closeIdx < 0 then (Nil, trimmed)
      else
        val extAttr = parseExtAttrList(trimmed.substring(1, closeIdx))
        (extAttr, trimmed.substring(closeIdx + 1).trim)

  /** finds the `[...]` extended attribute list immediately preceding `defStart`
    * (i.e. nothing but whitespace between the `]` and the
    * `interface`/`namespace` keyword), if any — e.g. `[LegacyNamespace=
    * WebAssembly, Exposed=*]` before `interface Module {`. Scans backward with
    * bracket-depth counting since the list can itself hold nested `[]` (not
    * seen in this corpus, but WebIDL allows it in general).
    */
  private def extAttrBefore(
    block: String,
    defStart: Int,
  ): List[ExtendedAttribute] =
    val before = block.substring(0, defStart).trim
    if !before.endsWith("]") then Nil
    else
      var depth = 0
      var i = before.length - 1
      var openIdx = -1
      while i >= 0 && openIdx < 0 do
        before(i) match
          case ']' => depth += 1
          case '[' =>
            depth -= 1
            if depth == 0 then openIdx = i
          case _ =>
        i -= 1
      if openIdx < 0 then Nil
      else parseExtAttrList(before.substring(openIdx + 1, before.length - 1))

  /** parses the comma-separated content of a `[...]` extended attribute list,
    * e.g. `LegacyNamespace=WebAssembly, Exposed=*` or
    * `Exposed=(Window,Worker,Worklet)` (the latter has a single attribute whose
    * value itself contains commas, so splitting must ignore commas nested
    * inside `()`/`[]`)
    */
  private def parseExtAttrList(content: String): List[ExtendedAttribute] =
    splitTopLevel(content, ',')
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(_.split("=", 2).map(_.trim) match
        case Array(id)        => ExtendedAttribute(id, None)
        case Array(id, value) => ExtendedAttribute(id, Some(value))
        case _                => throw new MatchError(content),
      )

  /** splits `s` on every occurrence of `sep` that isn't nested inside
    * `()`/`[]`/`<>` — e.g. splitting `Exposed=(Window,Worker,Worklet)` on `,`
    * yields it whole, since its only comma is inside the parens
    */
  private def splitTopLevel(s: String, sep: Char): List[String] =
    val parts = List.newBuilder[String]
    val cur = new StringBuilder
    var depth = 0
    for ch <- s do
      ch match
        case '(' | '[' | '<' =>
          depth += 1
          cur += ch
        case ')' | ']' | '>' =>
          depth -= 1
          cur += ch
        case c if c == sep && depth == 0 =>
          parts += cur.toString
          cur.clear()
        case c => cur += c
    parts += cur.toString
    parts.result()

  /** splits `s` on top-level whitespace, i.e. whitespace not nested inside
    * `()`/`[]`/`<>` — e.g. `(DOMString or undefined) stack` splits into
    * `["(DOMString or undefined)", "stack"]`, not on the spaces inside the
    * union type
    */
  private def splitTopLevelWhitespace(s: String): List[String] =
    val parts = List.newBuilder[String]
    val cur = new StringBuilder
    var depth = 0
    for ch <- s do
      ch match
        case '(' | '[' | '<' =>
          depth += 1
          cur += ch
        case ')' | ']' | '>' =>
          depth -= 1
          cur += ch
        case c if c.isWhitespace && depth == 0 =>
          if cur.nonEmpty then
            parts += cur.toString
            cur.clear()
        case c => cur += c
    if cur.nonEmpty then parts += cur.toString
    parts.result()

  /** finds the index of the `(` matching the `)` at `closeIdx`, scanning
    * backward with paren-depth counting (mirrors [[findBodyEnd]], but for `()`
    * instead of `{}` and searching backward instead of forward)
    */
  private def findMatchingOpen(s: String, closeIdx: Int): Int =
    var depth = 0
    var i = closeIdx
    while i >= 0 do
      s(i) match
        case ')' => depth += 1
        case '(' =>
          depth -= 1
          if depth == 0 then return i
        case _ =>
      i -= 1
    -1
