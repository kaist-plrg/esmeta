package esmeta.wji.lang

import Expr.*
import Cond.*

/** Parses raw spec prose condition strings into [[Cond]] trees, falling back to
  * [[Cond.Unknown]] for unrecognized patterns.
  */
object CondParser:
  import TextSplit.*

  private val IsTypePos = """(?si)^(.+)\s+\[=is an?\s+([^\]]+)=\]$""".r
  private val IsTypeNeg = """(?si)^(.+)\s+\[=is not an?\s+([^\]]+)=\]$""".r
  private val MatchesNeg =
    """(?s)^(.+?)\s+does not\s+(\[=matches/[^\]]*\])\s+(.+)$""".r
  private val MatchesPos = """(?s)^(.+?)\s+(\[=matches/[^\]]*\])\s+(.+)$""".r
  private val MatchesType = """^\[=matches/([^\]|]+)""".r

  private val MapExistsPos = """(?si)^(.*?)\s+\[=map/exists=\]$""".r
  private val MapExistsNeg =
    """(?si)^(.*?)\s+(?:\[=map/doesn't exist=\]|doesn't \[=map/exist=\])$""".r
  // e.g. "|module|.[=imports=] [=list/is empty|is not empty=]" — the
  // `|alias=]` part is display text, not decoration: it's how the spec
  // writes the negated form ("is not empty") while still linking to the
  // "list/is empty" dfn, so its presence (and whether it reads "not") is what
  // determines polarity, not a separate positive/negative pattern pair.
  private val ListIsEmpty =
    """(?si)^(.+)\s+\[=list/is empty(?:\|([^\]]+))?=\]$""".r
  private val ImplementsPos =
    """(?si)^(.*?)\s+\[=implements=\]\s+\{\{([^}]+)\}\}$""".r
  private val ImplementsNeg =
    """(?si)^(.*?)\s+does not \[=implement=\]\s+\{\{([^}]+)\}\}$""".r
  // "EXPR has a [[SLOT]] internal slot" / "EXPR does not have a [[SLOT]]
  // internal slot" — checks whether an object has (been initialized with) a
  // particular internal slot, as opposed to ExprParser's PossessiveSlot
  // ("the value of X's [[slot]] internal slot"), which reads an existing
  // slot's value. The "internal slot" suffix may be plain text or a Bikeshed
  // dfn link (`[=internal slot=]`/`[=/internal slot=]`).
  private val HasSlotPos =
    """(?si)^(.+?)\s+has\s+an?\s+\\?\[\[([^\]]+)\]\]\s+(?:\[=/?internal slot=\]|internal slot)$""".r
  private val HasSlotNeg =
    """(?si)^(.+?)\s+does not have\s+an?\s+\\?\[\[([^\]]+)\]\]\s+(?:\[=/?internal slot=\]|internal slot)$""".r
  // "X contains any duplicates" / "X contains no duplicates" / "X does not
  // contain any duplicates" — see index.bs:1863.
  private val ContainsDuplicatesNeg =
    """(?si)^(.+?)\s+(?:contains no duplicates|does not contain any duplicates)$""".r
  private val ContainsDuplicatesPos =
    """(?si)^(.+?)\s+contains any duplicates$""".r
  // "[=algo|display=] for ARG1 [with ARG2[, ...] [and ARGN]] IS/RETURNS BOOL"
  // — a spec call phrased with "for"/"with"/"and" as its own English
  // argument-list connectors (mirrors the "from X, enabled Y, and Z" phrasing
  // an algorithm's own <dfn> head uses for its parameter list), immediately
  // compared against a boolean result. Matched as a whole *before* the
  // generic and/or top-level split in `parse` below, since that split can't
  // tell this "and" apart from a real boolean and — splitting first severs
  // the last argument from its call (see index.bs:411,423,455,765, all
  // "... for |module| with |builtinSetNames| and |importedStringModule|
  // returns/is false"). Restricted to a pure `|var|`-only argument list (no
  // free text) so it can't accidentally swallow a genuine "COND1 and COND2"
  // where COND1 itself happens to read "... for X is Y".
  private val LinkCallArgsEndsBool =
    """(?si)^(\[=[^\]]+\])\s+((?:for|with)\s+\|[^|]+\|(?:\s*(?:,\s*|and\s+|with\s+)\|[^|]+\|)*)\s+(is not|is|returns)\s+(true|false)$""".r

  // "contained in LIST" — the RHS of "ELEM is [not] contained in LIST"
  // (index.bs:1254), handled by parseRhs alongside "missing"/"given"/"of the
  // form ...".
  private val ContainedIn = """(?si)^contained in (.+)$""".r

  private val UnreachableStep = """(?si)^this step is not reached$""".r
  // "If this [operation] throws an exception, ..." (untyped) or
  // "If this [operation] throws a {{TypeError}}, ..." (typed): group 1 is the
  // exception type name when the typed form matched, else null.
  private val ThrowsException =
    """(?si)^this(?: operation)? throws (?:an exception|an? \{\{([^}]+)\}\})$""".r

  // Or has lower precedence than And, so we split by Or first
  private val IsOfFormRhs = """(?si)^of the form (.+)$""".r

  // "any |t| in |parameters| or |results| [=matches/valtype|matches=]
  // [=v128=] or [=exnref=]" — an existential quantifier over one or more
  // collections. Checked before `parse`'s own top-level " or " splitting
  // below: naively splitting at the *first* " or " here would cut between
  // "parameters" and "results" — a collection-level "or" *nested inside*
  // "any ... in ...", not a top-level condition-level "or" the way that
  // splitter assumes. `predTail` (starting at the first `[=link=]` after the
  // collection list) is re-parsed with `binder` prepended as its elided
  // subject, e.g. "|t| [=matches/valtype|matches=] [=v128=] or [=exnref=]" —
  // that recursive `parse` call is what actually resolves the *second* "or"
  // (via the ordinary `Abbreviated`/`ExpandAbbreviatedCondPass` mechanism,
  // same as every other bare "X matches/T Y or Z" site in this file).
  // Requires each collection to be a bare `|var|` (every site reached so far
  // is) and the predicate to open with a `[=link=]`; narrow on purpose,
  // matching this file's other single-purpose patterns.
  private val AnyIn =
    """(?si)^any\s+(\S+)\s+in\s+((?:\|[^|]+\|)(?:\s+or\s+\|[^|]+\|)*)\s+(\[=.+)$""".r

  private val CompareOps = Seq(
    " >= ",
    " <= ",
    " ≥ ",
    " ⩾ ",
    " > ",
    " < ",
    " &gt;= ",
    " &lt;= ",
    " &gt; ",
    " &lt; ",
  )
  private val NormalizeOp: Map[String, CompareOp] = Map(
    " >= " -> CompareOp.Ge,
    " <= " -> CompareOp.Le,
    " > " -> CompareOp.Gt,
    " < " -> CompareOp.Lt,
    " ≥ " -> CompareOp.Ge,
    " ⩾ " -> CompareOp.Ge,
    " &gt;= " -> CompareOp.Ge,
    " &lt;= " -> CompareOp.Le,
    " &gt; " -> CompareOp.Gt,
    " &lt; " -> CompareOp.Lt,
  )

  def parse(raw: String): Cond =
    val s = raw.trim.stripSuffix(".")
    s match
      case LinkCallArgsEndsBool(link, argsPhrase, isKind, boolStr) =>
        Eq(
          ExprParser.parse(s"$link $argsPhrase"),
          Bool(boolStr.equalsIgnoreCase("true")),
          negated = isKind.trim.equalsIgnoreCase("is not"),
        )
      case AnyIn(binder, collsRaw, predTail) =>
        val collections = collsRaw.split("""\s+or\s+""").toList.map { c =>
          ExprParser.parse(c.trim)
        }
        Exists(binder, collections, parse(s"|$binder| $predTail"))
      case _ =>
        // Or has lower precedence than And, so we try it first
        findTopLevel(s, " or ") match
          case Some(i) =>
            Or(
              parse(s.substring(0, i).trim),
              parseOrAbbreviated(s.substring(i + 4).trim),
            )
          case None =>
            findTopLevel(s, " and ") match
              case Some(i) =>
                val left = s.substring(0, i).trim.stripSuffix(",").trim
                val right = s.substring(i + 5).trim
                And(parse(left), parseOrAbbreviated(right))
              case None => parseAtomic(s)

  /** Tries full condition parse; if it falls back to [[Unknown]], attempts to
    * salvage the text as an [[Abbreviated]] when [[ExprParser]] recognises it.
    */
  private def parseOrAbbreviated(s: String): Cond =
    parse(s) match
      case Cond.Unknown(text) =>
        ExprParser.parse(text) match
          case Expr.Unknown(_) => Cond.Unknown(text)
          case expr            => Abbreviated(expr)
      case cond => cond

  private def matchType(link: String): String =
    MatchesType.findFirstMatchIn(link).map(_.group(1)).getOrElse(link)

  private def parseAtomic(s: String): Cond = s match
    case UnreachableStep()     => Unreachable
    case ThrowsException(kind) => Throws(Option(kind))
    case MapExistsPos(baseRaw) => HasField(ExprParser.parse(baseRaw))
    case MapExistsNeg(baseRaw) =>
      HasField(ExprParser.parse(baseRaw), negated = true)
    case ListIsEmpty(baseRaw, alias) =>
      val negated = Option(alias).exists(_.toLowerCase.contains("not"))
      Eq(Length(ExprParser.parse(baseRaw.trim)), Num("0"), negated)
    case ImplementsPos(exprRaw, face) =>
      Implements(ExprParser.parse(exprRaw), face)
    case ImplementsNeg(exprRaw, face) =>
      Implements(ExprParser.parse(exprRaw), face, negated = true)
    case HasSlotPos(exprRaw, slot) =>
      HasSlot(ExprParser.parse(exprRaw.trim), slot)
    case HasSlotNeg(exprRaw, slot) =>
      HasSlot(ExprParser.parse(exprRaw.trim), slot, negated = true)
    case ContainsDuplicatesNeg(exprRaw) =>
      HasDuplicates(ExprParser.parse(exprRaw.trim), negated = true)
    case ContainsDuplicatesPos(exprRaw) =>
      HasDuplicates(ExprParser.parse(exprRaw.trim))
    case _ => parseEqOrCompare(s)

  private def parseEqOrCompare(s: String): Cond =
    def parseIsOfForm(lhsRaw: String, rhsText: String, negated: Boolean): Cond =
      val (formRaw, condOpt) = splitTopLevel(rhsText.trim, " where ") match
        case Some((f, c)) => (f.trim, Some(parse(c.trim)))
        case None         => (rhsText.trim, None)
      IsOfForm(
        ExprParser.parse(lhsRaw),
        ExprParser.parse(formRaw),
        condOpt,
        negated,
      )

    def parseRhs(lhsRaw: String, rhsRaw: String, negated: Boolean): Cond =
      rhsRaw.trim match
        case "missing" => IsMissing(ExprParser.parse(lhsRaw.trim), negated)
        case "given"   => IsMissing(ExprParser.parse(lhsRaw.trim), !negated)
        case IsOfFormRhs(text) => parseIsOfForm(lhsRaw.trim, text, negated)
        case ContainedIn(listRaw) =>
          Contains(
            ExprParser.parse(lhsRaw.trim),
            ExprParser.parse(listRaw.trim),
            negated,
          )
        case _ =>
          Eq(
            ExprParser.parse(lhsRaw.trim),
            ExprParser.parse(rhsRaw.trim),
            negated,
          )

    splitTopLevel(s, " is not equal to ")
      .map {
        case (l, r) =>
          Eq(ExprParser.parse(l.trim), ExprParser.parse(r.trim), negated = true)
      }
      .orElse(splitTopLevel(s, " is equal to ").map {
        case (l, r) => Eq(ExprParser.parse(l.trim), ExprParser.parse(r.trim))
      })
      .orElse(splitTopLevel(s, " is not ").map {
        case (l, r) => parseRhs(l, r, negated = true)
      })
      .orElse(
        splitTopLevel(s, " is ")
          .filter { case (_, r) => !r.trim.startsWith("one of") }
          .map { case (l, r) => parseRhs(l, r, negated = false) },
      )
      .orElse(splitTopLevel(s, " does not equal ").map {
        case (l, r) =>
          Eq(ExprParser.parse(l.trim), ExprParser.parse(r.trim), negated = true)
      })
      .orElse(splitTopLevel(s, " equals ").map {
        case (l, r) => Eq(ExprParser.parse(l.trim), ExprParser.parse(r.trim))
      })
      .orElse(findTopLevelAny(s, CompareOps).map {
        case (i, op) =>
          Compare(
            ExprParser.parse(s.substring(0, i).trim),
            NormalizeOp(op),
            ExprParser.parse(s.substring(i + op.length).trim),
          )
      })
      .getOrElse(s match
        case IsTypeNeg(exprRaw, t) =>
          IsType(ExprParser.parse(exprRaw.trim), t.trim, negated = true)
        case IsTypePos(exprRaw, t) =>
          IsType(ExprParser.parse(exprRaw.trim), t.trim)
        case MatchesNeg(l, link, r) =>
          Matches(
            ExprParser.parse(l.trim),
            matchType(link),
            ExprParser.parse(r.trim),
            negated = true,
          )
        case MatchesPos(l, link, r) =>
          Matches(
            ExprParser.parse(l.trim),
            matchType(link),
            ExprParser.parse(r.trim),
          )
        case _ => Cond.Unknown(s),
      )
