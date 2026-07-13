package esmeta.wji.lang

import scala.collection.mutable.ListBuffer
import java.nio.file.{Files, Path}

/** Extracts [[Algorithm]]s out of `<div algorithm>` blocks in the Bikeshed
  * source of the WebAssembly JS API specification
  * (`spectec/document/js-api/index.bs`).
  *
  * The extractor works directly on the `.bs` source text rather than on
  * compiled HTML: algorithm steps are written as (possibly nested)
  * Markdown-style lists, e.g.
  *
  * {{{
  * <div algorithm>
  *   To <dfn>compile a WebAssembly module</dfn> from source bytes |bytes|,
  *   perform the following steps:
  *     1. Let |module| be [=module_decode=](|bytes|). If |module| is
  *         [=error=], return [=error=].
  *     1. If [=module_validate=](|module|) is [=error=], return [=error=].
  *     1. Return |module|.
  * </div>
  * }}}
  */
object AlgorithmExtractor:

  /** matches the opening `<div algorithm...>` tag, capturing the optional
    * `algorithm="..."` / `algorithm='...'` / `algorithm=...` attribute value
    */
  private val OpenTag =
    """(?s)<div\s+algorithm(?:\s*=\s*(?:"([^"]*)"|'([^']*)'|([^\s>]+)))?\s*>""".r

  /** matches `<div ...>` and `</div>` tags, used to find the `</div>` matching
    * an algorithm's opening tag while accounting for any divs nested inside
    */
  private val DivTag = """(?is)<div\b[^>]*>|</div\s*>""".r

  /** matches a (possibly nested) ordered (`1.`) or unordered (`*`) list item,
    * capturing its indentation and content
    */
  private val StepMarker = """^(\s*)(?:\d+\.|\*)\s+(.*)$""".r

  /** matches the first `<dfn ...>...</dfn>` in a string */
  private val Dfn = """(?is)<dfn\b[^>]*>(.*?)</dfn>""".r

  /** matches the `lt` attribute in a `<dfn>` opening tag (double or single
    * quotes)
    */
  private val DfnLtAttr = """(?is)\blt\s*=\s*(?:"([^"]*)"|'([^']*)')""".r

  /** matches a Bikeshed variable reference, e.g. `|bytes|` or `|importObject|`
    */
  private val PipeVar = """\|[A-Za-z][A-Za-z0-9]*\|""".r

  /** matches a type parameter written inside a Bikeshed generic angle-bracket
    * instantiation, e.g. the `T` in `<code>Promise&lt;<var
    * ignore>T</var>&gt;</code>` (webidl's `a new promise`). Unlike an ordinary
    * `<var ignore>...</var>` used elsewhere in prose as a pattern-match
    * wildcard (e.g. "of the form ... <var ignore>mut</var> |valtype|", which
    * must keep being ignored), one written inside a `&lt;...&gt;` generic slot
    * names a real type parameter of the operation being defined — the same role
    * a `|T|` pipe var plays when the spec writes the generic slot with a real
    * bound variable instead (e.g. `resolve`'s `Promise&lt;|T|&gt;`, already
    * covered by [[PipeVar]]). See [[ExprParser.OfTypeGeneric]] for the
    * call-site counterpart that instantiates it.
    */
  private val GenericVarIgnore =
    """&lt;\s*<var\s+ignore>([A-Za-z][A-Za-z0-9]*)</var>\s*&gt;""".r

  /** matches either a [[PipeVar]] or a [[GenericVarIgnore]] token, so
    * [[extractParams]] can scan both kinds of parameter left-to-right in a
    * single pass and preserve their relative order.
    */
  private val ParamToken =
    """\|[A-Za-z][A-Za-z0-9]*\||&lt;\s*<var\s+ignore>[A-Za-z][A-Za-z0-9]*</var>\s*&gt;""".r

  /** matches a trailing parameter list on a `<dfn>` inner text, where params
    * use Bikeshed `|variable|` syntax, e.g. `validate(|bytes|, |options|)`
    */
  private val TrailingParams =
    """^(.+?)\s*\((?:\s*\|[A-Za-z][A-Za-z0-9]*\|\s*(?:,\s*\|[A-Za-z][A-Za-z0-9]*\|\s*)*)?\)$""".r

  /** matches a trailing parameter list on an `lt` attribute value, where params
    * are plain identifiers (no pipes), e.g. `Tag(type)` or
    * `Exception(exceptionTag, payload, options)`
    */
  private val TrailingParamsPlain = """^(.+?)\s*\([^)]*\)$""".r

  /** matches a `<dfn method|attribute|constructor for="...">` opening tag,
    * capturing the kind word and the `for` value (double or single quotes) —
    * see [[AlgorithmKind]].
    */
  private val DfnKindFor =
    """(?is)<dfn\s+(method|attribute|constructor)\s+for\s*=\s*(?:"([^"]*)"|'([^']*)')""".r

  /** matches "The setter of the ... attribute of {{Interface}}" — a setter
    * never has its own `<dfn attribute for=...>` (see
    * [[AlgorithmKind.Setter]]), so it's recognized from this prose pattern
    * instead, capturing the interface name out of the `{{...}}` IDL reference.
    */
  private val SetterOfAttribute =
    """(?is)^\s*The setter of the .+? attribute of \{\{([^}]+)\}\}""".r

  def extract(source: String): List[Algorithm] =
    OpenTag.findAllMatchIn(source).toList.flatMap { m =>
      val id =
        Option(m.group(1)).orElse(Option(m.group(2))).orElse(Option(m.group(3)))
      for bodyEnd <- findBodyEnd(source, m.end) yield
        val body = source.substring(m.end, bodyEnd)
        val (head, instrs) = parseBody(body)
        Algorithm(
          id,
          extractName(head),
          extractParams(head),
          head,
          instrs,
          extractKind(head),
        )
    }

  def extractFromFile(path: Path): List[Algorithm] =
    extract(SpecPatch(Files.readString(path)))

  /** finds the index of the `</div>` matching the `<div algorithm...>` tag
    * whose body starts at `bodyStart`, accounting for any `<div>...</div>`
    * nested inside
    */
  private def findBodyEnd(source: String, bodyStart: Int): Option[Int] =
    var depth = 1
    val tokens = DivTag.findAllMatchIn(source.substring(bodyStart))
    while tokens.hasNext do
      val tok = tokens.next()
      depth += (if tok.matched.startsWith("</") then -1 else 1)
      if depth == 0 then return Some(bodyStart + tok.start)
    None

  /** splits the content of a `<div algorithm>` block into its leading
    * descriptive text (the "head") and its list of top-level steps,
    * reconstructing the nesting of sub-steps from indentation
    */
  private def parseBody(body: String): (String, List[Instr]) =
    final class Builder(var text: String):
      val subSteps = ListBuffer[Builder]()
      def toInstrs: List[Instr] =
        InstrParser.parseStepText(text, subSteps.flatMap(_.toInstrs).toList)

    val headLines = ListBuffer[String]()
    val roots = ListBuffer[Builder]()
    // stack of (indentation, step) for steps that may still receive
    // nested sub-steps or continuation lines
    val stack = ListBuffer[(Int, Builder)]()

    for line <- body.linesIterator do
      line match
        case _ if line.isBlank =>
        case StepMarker(indent, rest) =>
          val step = new Builder(rest.trim)
          while stack.nonEmpty && stack.last._1 >= indent.length do
            stack.remove(stack.length - 1)
          if stack.isEmpty then roots += step
          else stack.last._2.subSteps += step
          stack += (indent.length -> step)
        case other =>
          val text = other.trim
          if stack.isEmpty then headLines += text
          else
            val top = stack.last._2
            top.text = s"${top.text} $text".trim

    (headLines.mkString(" ").trim, roots.flatMap(_.toInstrs).toList)

  /** name from the first `<dfn>` in `head`.
    *
    * Prefers the `lt` attribute (Bikeshed "linking text") over the inner text,
    * because `[=a new promise=]` links to `lt="a new promise"`, not to the
    * display text ("create"). When `lt` has multiple `|`-separated values, the
    * first one is used. Falls back to the inner text (with any trailing
    * parameter list stripped) when no `lt` is present.
    */
  private def extractName(head: String): Option[String] =
    Dfn.findFirstMatchIn(head).map { m =>
      val openTag = m.matched.substring(0, m.matched.indexOf('>') + 1)
      DfnLtAttr.findFirstMatchIn(openTag) match
        case Some(ltMatch) =>
          val ltValue = Option(ltMatch.group(1))
            .orElse(Option(ltMatch.group(2)))
            .getOrElse("")
          val primary = ltValue.split('|').head.trim
          primary match
            case TrailingParamsPlain(name) => name.trim
            case name                      => name
        case None =>
          m.group(1).trim match
            case TrailingParams(name) => name.trim
            case name                 => name
    }

  /** distinct formal parameters in `head`, in order of first appearance — both
    * ordinary `|variable|` references and generic-bracket type parameters (see
    * [[GenericVarIgnore]]), the latter normalized to pipe form (`|T|`) so every
    * entry in [[Algorithm.params]] has a uniform representation regardless of
    * which surface syntax declared it.
    */
  private def extractParams(head: String): List[String] =
    ParamToken
      .findAllMatchIn(head)
      .map { m =>
        val text = m.matched
        if text.startsWith("|") then text
        else s"|${GenericVarIgnore.findFirstMatchIn(text).get.group(1)}|"
      }
      .toList
      .distinct

  /** what `head` declares this algorithm to implement — see [[AlgorithmKind]].
    */
  private def extractKind(head: String): AlgorithmKind =
    DfnKindFor.findFirstMatchIn(head) match
      case Some(m) =>
        val iface =
          Option(m.group(2)).orElse(Option(m.group(3))).getOrElse("")
        m.group(1).toLowerCase match
          case "method"      => AlgorithmKind.Method(iface)
          case "attribute"   => AlgorithmKind.Getter(iface)
          case "constructor" => AlgorithmKind.Constructor(iface)
          case _             => AlgorithmKind.Plain
      case None =>
        SetterOfAttribute.findFirstMatchIn(head) match
          case Some(m) => AlgorithmKind.Setter(m.group(1).trim)
          case None    => AlgorithmKind.Plain
