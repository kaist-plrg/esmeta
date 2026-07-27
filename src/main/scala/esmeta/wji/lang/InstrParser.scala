package esmeta.wji.lang

import Instr.*
import Instr.PerformOutcome.*

/** Converts the raw step text produced by [[AlgorithmExtractor]] into an
  * [[Instr]] AST.
  */
object InstrParser:
  import TextSplit.*

  private val ElseClauseStart = """(?is)^(else|otherwise)\b.*""".r

  private val LetPrefix = """(?is)^Let\b\s+(.+)$""".r
  private val SetPrefix = """(?is)^Set\b\s+(.+)$""".r
  private val AssertPrefix = """(?is)^Assert:\s*(.+)$""".r
  private val NotePrefix = """(?is)^Note:\s*(.*)$""".r
  private val ReturnPrefix = """(?is)^Return\b\.?\s*(.*)$""".r
  private val ThrowPrefix = """(?is)^(?:\[=[Tt]hrow=\]|Throw\b)\s+(.+)$""".r
  private val ElseIfPrefix =
    """(?is)^(?:Else\s+if\b|Otherwise,\s*if\b)\s+(.+)$""".r
  private val IfPrefix = """(?is)^If\b\s+(.+)$""".r
  private val ElsePrefix = """(?is)^(?:Else|Otherwise)\b\s*,?\s*(.*)$""".r
  private val ForEachPrefix =
    """(?is)^(?:\[=\S*\|)?For each(?:=\])?\s+(.+)$""".r
  private val ForPrefix =
    """(?is)^For\s+(\|[^|]+\|)\s+in\s+(.+)$""".r
  private val WhilePrefix = """(?is)^While\b\s+(.+)$""".r
  private val AppendPrefix = """(?is)^\[=list/Append=\]\s+(.+)$""".r
  private val MapSetPrefix = """(?is)^\[=map/[Ss]et[^=]*=\]\s+(.+)$""".r
  private val IterationContinuePrefix = """(?is)^\[=iteration/continue=\]$""".r
  private val RunInParallelPrefix =
    """(?is)^Run the following steps\b.*\bin parallel\b.*$""".r
  private val PerformPrefix = """(?is)^(?:Perform\s+)?(\[[=$].+|Run\b.+)$""".r
  private val PerformAndReturnSuffix =
    """(?is)^(.*?),?\s+and\s+return\s+the\s+result$""".r
  private val PerformAndLetSuffix =
    """(?is)^(.*?),?\s+and\s+let\s+(\|[^|]+\|)\s+be\s+the\s+result$""".r
  private val PerformAndStoreSuffix =
    """(?is)^(.*?),?\s+and\s+store\s+the\s+results?\s+as\s+(\|[^|]+\|)\s*$""".r
  // a `Perform` step chained with a bare early return, e.g. "reject |promise|
  // with a {{CompileError}} exception and return." — distinct from
  // PerformAndReturnSuffix above, which only matches "...and return **the
  // result**" (chaining the call's own return value); this is an
  // unconditional `Return.` with no value, unrelated to what the call
  // produced.
  private val PerformAndBareReturnSuffix =
    """(?is)^(.*?),?\s+and\s+return$""".r

  // matches the leading [=..=] algo-link and the rest of the expression
  private val LeadingAlgoLink = """(?s)^(\[=[^\]]+\])\s*(.*)$""".r

  // a `Perform` step whose target is a raw ECMA-262/WebIDL AO call, e.g.
  // `Perform [$SetIntegrityLevel$](...)`, rather than a WJI `[=algo=]` link —
  // mirrors ExprParser's JSCallFull. Must be tried before LeadingAlgoLink,
  // whose `[^\]]+` would otherwise swallow the `$...$` name as if it were an
  // algo-link (a `[$...$]` call has no `]` before its own closing bracket, so
  // LeadingAlgoLink's pattern only fails to match at all — it's not that it
  // matches wrong, just that this shape needs its own case).
  private val LeadingJSCall = """(?s)^\[\$([^$]+)\$\]\((.*)\)$""".r

  // `[=!=]`/`[=?=]` ("this call must not complete abruptly" / "propagate an
  // abrupt completion") prefixing a `Perform` step's call, e.g. `Perform
  // [=!=] [$SetIntegrityLevel$](...)`. Mirrors ExprParser's AbruptPrefix —
  // without this, `[=!=]` is indistinguishable from a real `[=algo=]` link to
  // LeadingAlgoLink below, and gets parsed as one (a call to a function named
  // "!"). Perform has no abrupt-check field to carry the marker to, so — like
  // Compiler's own `Abrupt("!", e)` handling — it's simply discarded here.
  private val AbruptCallPrefix = """(?s)^\[=[?!]=\]\s+(.+)$""".r

  // e.g. "catch it, [=reject=] |promise| with the exception, and return
  // |promise|" — the body of an `If(Cond.Throws, ...)`. Written as one
  // comma/"and"-joined sentence (no periods), so it can't go through the
  // normal per-sentence splitting; "catch it" itself carries no meaning of
  // its own (`Cond.Throws` already implies the `|exception|` binding).
  private val CatchItPrefix = """(?si)^catch it\s*,\s*(.+)$""".r

  private def parseCall(expr: String): (String, List[Expr]) = expr.trim match
    case AbruptCallPrefix(rest) => parseCall(rest)
    case LeadingJSCall(name, argsRaw) =>
      (name, splitComma(argsRaw).map(ExprParser.parse))
    case LeadingAlgoLink(func, rest) =>
      (ExprParser.normalizeLink(func), ExprParser.parseArgs(rest))
    case _ => (expr, Nil)

  /** converts the text of a single numbered/bulleted list item - which may
    * contain more than one sentence, e.g. `"Let |x| be Y. If |x| is Z, return
    * Z."` - into one or more [[Instr]]s, attaching `subInstrs` (the item's own
    * nested list items, already converted) as the body of the last one
    */
  def parseStepText(text: String, subInstrs: List[Instr]): List[Instr] =
    parseSentences(text, subInstrs)

  /** splits `text` into sentences, classifies each one, and attaches
    * `trailingBody` (converted from a step's nested sub-steps, if any) to the
    * last sentence.
    *
    * A `Note: ...` sentence absorbs every sentence after it into a single
    * [[Note]], since such asides are written as one or more trailing sentences
    * of free-form prose rather than as separate steps (e.g. `"Note: Foo bar.
    * Baz qux."` is one [[Note]] with text `"Foo bar. Baz qux"`, not a [[Note]]
    * followed by an [[Unknown]]).
    */
  private def parseSentences(
    text: String,
    trailingBody: List[Instr],
  ): List[Instr] =
    text.trim match
      case CatchItPrefix(rest) =>
        val pieces = splitComma(rest)
        val actions = pieces.zipWithIndex.map { (piece, i) =>
          val body = if i == pieces.length - 1 then trailingBody else Nil
          parseSentence(stripLeadingAnd(piece), body)
        }
        Note("catch it") :: actions
      case trimmed =>
        val sentences = splitSentences(trimmed).filter(_.nonEmpty)
        sentences match
          case Nil if trailingBody.isEmpty => Nil
          case Nil                         => List(Unknown("", trailingBody))
          case _ =>
            sentences.indexWhere(NotePrefix.matches) match
              case -1 =>
                sentences.init.map(parseSentence(_, Nil)) :+ parseSentence(
                  sentences.last,
                  trailingBody,
                )
              case noteIdx =>
                val before =
                  sentences.take(noteIdx).map(parseSentence(_, Nil))
                before :+ parseSentence(
                  sentences.drop(noteIdx).mkString(" "),
                  trailingBody,
                )

  private def stripLeadingAnd(s: String): String =
    val t = s.trim
    if t.toLowerCase.startsWith("and ") then t.drop(4).trim else t

  /** splits `text` into sentence-like fragments: at every top-level `". "`, and
    * further at a top-level `"; "` when it introduces an `else`/`otherwise`
    * clause (e.g. `"..., let X be Y; otherwise, let X be Z."`)
    */
  private def splitSentences(text: String): List[String] =
    splitTopLevelAll(text, ". ").flatMap(splitElseClause)

  private def splitElseClause(fragment: String): List[String] =
    findTopLevel(fragment, "; ") match
      case Some(i) =>
        val before = fragment.substring(0, i)
        val after = fragment.substring(i + 2)
        if ElseClauseStart.matches(after) then List(before, after)
        else List(fragment)
      case None => List(fragment)

  private def parseSentence(raw: String, trailingBody: List[Instr]): Instr =
    val text = raw.trim.stripSuffix(".").trim
    text match
      case LetPrefix(rest) =>
        splitTopLevel(rest, " be ") match
          case Some((lhs, expr)) =>
            Let(
              ExprParser.parseUntaggedForm(lhs),
              ExprParser.parse(expr),
              trailingBody,
            )
          case None => Unknown(text, trailingBody)
      case SetPrefix(rest) =>
        splitTopLevel(rest, " to ").orElse(
          splitTopLevel(rest, " as specified in "),
        ) match
          case Some((lhs, expr)) =>
            Set(
              ExprParser.parse(lhs),
              ExprParser.parse(expr),
              trailingBody,
            )
          case None => Unknown(text, trailingBody)
      case AssertPrefix(cond) =>
        Assert(CondParser.parse(cond), trailingBody)
      case NotePrefix(note) => Note(note.trim, trailingBody)
      case ReturnPrefix(expr) =>
        Return(
          Option.when(expr.nonEmpty)(ExprParser.parse(expr)),
          trailingBody,
        )
      case ThrowPrefix(target) => Throw(target.trim, trailingBody)
      case ElseIfPrefix(rest) =>
        val (cond, tail) = splitCondAndRest(rest)
        ElseIf(CondParser.parse(cond), deriveBody(tail, trailingBody))
      case IfPrefix(rest) =>
        val (cond, tail) = splitCondAndRest(rest)
        If(CondParser.parse(cond), deriveBody(tail, trailingBody))
      case ElsePrefix(rest) => Else(deriveBody(rest.trim, trailingBody))
      case ForEachPrefix(rest) =>
        findTopLevelAny(rest, Seq(" of ", " in ")) match
          case Some((i, sep)) =>
            val elem = rest.substring(0, i).trim
            val after = rest.substring(i + sep.length)
            val collection = findTopLevel(after, ",") match
              case Some(j) => after.substring(0, j).trim
              case None    => after.stripSuffix(",").trim
            ForEach(
              ExprParser.parse(elem),
              ExprParser.parse(collection),
              trailingBody,
            )
          case None => Unknown(text, trailingBody)
      case ForPrefix(elemStr, rest) =>
        val (collection, bodyText) = splitForCollection(rest)
        For(
          ExprParser.parse(elemStr),
          ExprParser.parse(collection),
          deriveBody(bodyText, trailingBody),
        )
      case WhilePrefix(rest) =>
        While(CondParser.parse(rest.stripSuffix(":")), trailingBody)
      case AppendPrefix(rest) =>
        splitTopLevel(rest, " to ") match
          case Some((item, collection)) =>
            Append(
              ExprParser.parse(item),
              ExprParser.parse(collection),
              trailingBody,
            )
          case None => Unknown(text, trailingBody)
      case MapSetPrefix(rest) =>
        splitTopLevel(rest, " to ") match
          case Some((lhs, expr)) =>
            Set(
              ExprParser.parse(lhs),
              ExprParser.parse(expr),
              trailingBody,
            )
          case None => Unknown(text, trailingBody)
      case _ if IterationContinuePrefix.matches(text) => Continue(trailingBody)
      case _ if RunInParallelPrefix.matches(text) => RunInParallel(trailingBody)
      case PerformPrefix(expr)                    =>
        // each suffix only changes the outcome/trailing body, not how the
        // call itself is parsed — so determine those first, then call
        // parseCall exactly once instead of once per suffix case.
        val (op, outcome, body) = expr.trim match
          case PerformAndReturnSuffix(op) => (op, ReturnResult, trailingBody)
          case PerformAndLetSuffix(op, variable) =>
            (op, BindResult(variable), trailingBody)
          case PerformAndStoreSuffix(op, variable) =>
            (op, BindResult(variable), trailingBody)
          case PerformAndBareReturnSuffix(op) =>
            (op, Discard, Return(None) :: trailingBody)
          case op => (op, Discard, trailingBody)
        val (func, args) = parseCall(op)
        Perform(func, args, outcome, body)
      case _ => Unknown(text, trailingBody)

  /** splits the text following `If`/`Else if`/... into its condition and the
    * (possibly empty) remainder, at the first top-level `,` or `:`. A `,` that
    * is immediately followed by `"and "`/`"or "` is treated as part of a
    * multi-clause condition (e.g. `"If A, and B, do C."`) rather than the
    * cond/action separator. A leading `"then "` on the remainder (as in `"If X,
    * then Y."`) is stripped.
    */
  private def splitCondAndRest(text: String): (String, String) =
    def find(from: Int): Option[(Int, String)] =
      findTopLevelAny(text.substring(from), Seq(",", ":")).flatMap {
        case (i, sep) =>
          val pos = from + i
          val after = text.substring(pos + sep.length).trim.toLowerCase
          if sep == "," && (after.startsWith("and ") || after.startsWith(
              "or ",
            ))
          then find(pos + sep.length)
          else Some((pos, sep))
      }

    find(0) match
      case Some((i, sep)) =>
        val cond = text.substring(0, i).trim
        var rest = text.substring(i + sep.length).trim
        if sep == "," then
          if rest.toLowerCase.startsWith("then ") then rest = rest.drop(5).trim
          else if rest.equalsIgnoreCase("then") then rest = ""
        (cond, rest)
      case None => (text.trim, "")

  /** splits the text after `For ELEM in` into the collection expression and the
    * loop's inline body. The collection ends at the first top-level comma, but
    * a range's trailing ", inclusive" (e.g. "... to Y, inclusive, set ...")
    * describes the collection rather than starting the body, so it is stripped
    * before the remainder is handed back as the body text.
    */
  private def splitForCollection(rest: String): (String, String) =
    findTopLevel(rest, ",") match
      case Some(j) =>
        val collection = rest.substring(0, j).trim
        var body = rest.substring(j + 1).trim
        if body.toLowerCase.startsWith("inclusive") then
          val after = body.substring("inclusive".length)
          // guard against words like "inclusively": the marker must be followed
          // by a comma or the end of the text, not more letters
          if after.isEmpty || !after.head.isLetter then
            body = after.trim.stripPrefix(",").trim
        (collection, body)
      case None => (rest.stripSuffix(",").trim, "")

  /** the body of a block-introducing [[Instr]] (`If`, `ForEach`, ...): the
    * converted sub-steps if there are any, otherwise the remaining text
    * re-parsed as further [[Instr]]s
    */
  private def deriveBody(rest: String, trailingBody: List[Instr]): List[Instr] =
    if trailingBody.nonEmpty then trailingBody
    else if rest.nonEmpty then parseSentences(rest, Nil)
    else Nil
