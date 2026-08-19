package esmeta.wji.lang.parser

import esmeta.wji.lang.*

import Expr.*
import Cond.*

/** Parses raw spec prose condition strings into [[Cond]] trees, falling back to
  * [[Cond.Unknown]] for unrecognized patterns.
  */
object CondParser:
  import TextSplit.*

  private val IsTypePos = """(?si)^(.+)\s+\[=is an?\s+([^\]]+)=\]$""".r
  private val IsTypeNeg = """(?si)^(.+)\s+\[=is not an?\s+([^\]]+)=\]$""".r
  // "EXPR is a/an [=NOUN=]" — unlike IsTypePos above (where "is a X" is
  // itself one dfn-link, e.g. ECMA-262's own `[=is a Number=]` term), this is
  // plain "is a/an" prose linking only the noun (e.g. "|v| is an [=Exported
  // Function=]"). Grammatically the two are the same claim — English "X is a
  // NOUN" is always kind-membership, never value equality — so both parse to
  // the same `IsType`, letting a later lowering pass (not `Compiler`, which
  // only knows genuine ECMAScript types) decide what a WJI-specific NOUN like
  // "Exported Function" actually compiles to.
  private val ArticleLink = """(?si)^an?\s+\[=([^\]]+)=\]$""".r
  // "EXPR is [not] [=valid TYPE|valid=]" — Wasm Core's own validation dfns
  // (index.bs:877/1044's Memory/Table constructors), always written with a
  // `|valid` display-text alias since the dfn text itself ("valid memtype")
  // would otherwise render redundantly as "is valid valid". TYPE feeds
  // straight into `WasmHost`'s `valid_TYPE` embedding function names (see
  // that trait's own doc + `docs/hardcodes.md`) — no separate lookup table,
  // since both existing names already match this exact shape.
  private val ValidTypeLink = """(?si)^\[=valid\s+(\w+)(?:\|.*)?=\]$""".r
  private val MatchesNeg =
    """(?s)^(.+?)\s+does not\s+(\[=matches/[^\]]*\])\s+(.+)$""".r
  private val MatchesPos = """(?s)^(.+?)\s+(\[=matches/[^\]]*\])\s+(.+)$""".r
  private val MatchesType = """^\[=matches/([^\]|]+)""".r

  private val MapExistsPos = """(?si)^(.*?)\s+\[=map/exists=\]$""".r
  // "EXPR doesn't [=map/exist=]" / "EXPR [=map/doesn't exist=]" / "EXPR
  // [=map/exists=] is false" — three spellings of the same negation seen in
  // the corpus; the third (index.bs:1471) restates the positive
  // `[=map/exists=]` term rather than linking a dedicated negative dfn.
  private val MapExistsNeg =
    ("""(?si)^(.*?)\s+(?:\[=map/doesn't exist=\]|doesn't \[=map/exist=\]""" +
      """|\[=map/exists=\]\s+is\s+false)$""").r
  // "EXPR has been initialized" / "EXPR has not been initialized" — a lazily-
  // computed-and-cached per-agent field (index.bs:1795, "the surrounding
  // agent's associated JavaScript exception tag has been initialized"), same
  // HasField shape as MapExistsPos/Neg above: the field starts absent and
  // this checks presence, not any particular value.
  private val HasBeenInitializedPos =
    """(?si)^(.+?)\s+has been initialized$""".r
  private val HasBeenInitializedNeg =
    """(?si)^(.+?)\s+has not been initialized$""".r
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

  // "[=exposed=] in REALM" — the RHS of "SUBJECT is [not] [=exposed=] in
  // REALM" (webidl/index.bs:12276,12325,12523, and the interface-construction
  // "Assert: |interface| is [=exposed=] in |realm|" — every site in this
  // corpus writes the link with no display-text alias), handled by parseRhs
  // alongside "missing"/"given"/"of the form ...."/"contained in ...".
  // Dedicated rather than falling through to the generic `Eq` handling below
  // (which would otherwise compare `subject` against a `Link`/`AlgoCall`
  // value, a category error — "is exposed" is a predicate, not an equality)
  // so `Cond.Exposed` gets a real node instead of relying on the accidental
  // shape that generic fallback happens to produce.
  private val ExposedIn = """(?si)^\[=exposed=\]\s+in\s+(.+)$""".r

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

  // "a/an [=NOUN=] |binder| exists such that BODY" — a genuine existential
  // with no explicit search domain (contrast AnyIn above), e.g. "a [=host
  // address=] |hostaddr| exists such that |map|[|hostaddr|] is the same as
  // |v|" (index.bs:1469). Checked before `parse`'s own top-level " is "/
  // "or"/"and" splitting for the same reason as AnyIn: naive splitting would
  // otherwise cut this apart wrongly (the first top-level " is " here sits
  // *inside* `body`, not between some outer subject and this whole clause —
  // confirmed empirically: without this case, the trailing "[|hostaddr|]"
  // gets misread as an `Index` on the *whole* preceding clause, and "is the
  // same as |v|" splits off as if this were a top-level equality). `body`
  // (everything after "such that") already refers to `binder` via a real
  // `|binder|` pipe-var directly (unlike AnyIn's `predTail`, whose subject is
  // elided and must be reconstructed), so it's parsed as-is with no prefix
  // injection needed.
  private val ExistsSuchThat =
    """(?si)^an?\s+\[=[^\]]+=\]\s+\|(\w+)\|\s+exists\s+such\s+that\s+(.+)$""".r

  // single source of truth for every comparison-operator spelling (spec
  // prose writes both the literal symbol and its HTML-entity escape) — the
  // separator list `findTopLevelAny` scans and the op each one normalizes to
  // are derived from this pair list below, rather than kept as two
  // hand-synchronized `Seq`/`Map` literals that could silently drift apart.
  private val CompareOps: Seq[(String, CompareOp)] = Seq(
    " >= " -> CompareOp.Ge,
    " <= " -> CompareOp.Le,
    " ≥ " -> CompareOp.Ge,
    " ⩾ " -> CompareOp.Ge,
    " > " -> CompareOp.Gt,
    " < " -> CompareOp.Lt,
    " &gt;= " -> CompareOp.Ge,
    " &lt;= " -> CompareOp.Le,
    " &gt; " -> CompareOp.Gt,
    " &lt; " -> CompareOp.Lt,
  )
  private val CompareOpSeps: Seq[String] = CompareOps.map(_._1)
  private val NormalizeOp: Map[String, CompareOp] = CompareOps.toMap

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
          ExprParser.parse(c)
        }
        Any(binder, collections, parse(s"|$binder| $predTail"))
      case ExistsSuchThat(binder, body) =>
        Exists(binder, parse(body))
      case _ =>
        // A top-level " where " (e.g. "X is of the form Y where Z1 or Z2",
        // index.bs:1212) scopes everything after it to a nested
        // sub-condition — bound the or/and search below to end there, so an
        // "or"/"and" that's actually *inside* the where-clause (like "Z1 or
        // Z2" above) doesn't get mistaken for splitting the *whole* thing
        // into top-level siblings. `parseIsOfForm` below re-parses the
        // where-clause fresh once reached, correctly rescoped to just that
        // fragment.
        val searchIn = findTopLevel(s, " where ") match
          case Some(i) => s.substring(0, i)
          case None    => s
        // Or has lower precedence than And, so we try it first
        findTopLevel(searchIn, " or ") match
          case Some(i) =>
            Or(
              parse(s.substring(0, i)),
              parseOrAbbreviated(s.substring(i + 4).trim),
            )
          case None =>
            findTopLevel(searchIn, " and ") match
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
    case HasBeenInitializedPos(baseRaw) => HasField(ExprParser.parse(baseRaw))
    case HasBeenInitializedNeg(baseRaw) =>
      HasField(ExprParser.parse(baseRaw), negated = true)
    case ListIsEmpty(baseRaw, alias) =>
      val negated = Option(alias).exists(_.toLowerCase.contains("not"))
      Eq(Length(ExprParser.parse(baseRaw)), Num("0"), negated)
    case ImplementsPos(exprRaw, face) =>
      Implements(ExprParser.parse(exprRaw), face)
    case ImplementsNeg(exprRaw, face) =>
      Implements(ExprParser.parse(exprRaw), face, negated = true)
    case HasSlotPos(exprRaw, slot) =>
      HasSlot(ExprParser.parse(exprRaw), slot)
    case HasSlotNeg(exprRaw, slot) =>
      HasSlot(ExprParser.parse(exprRaw), slot, negated = true)
    case ContainsDuplicatesNeg(exprRaw) =>
      HasDuplicates(ExprParser.parse(exprRaw), negated = true)
    case ContainsDuplicatesPos(exprRaw) =>
      HasDuplicates(ExprParser.parse(exprRaw))
    case _ => parseEqOrCompare(s)

  private def parseEqOrCompare(s: String): Cond =
    def parseIsOfForm(lhsRaw: String, rhsText: String, negated: Boolean): Cond =
      val (formRaw, condOpt) = splitTopLevel(rhsText.trim, " where ") match
        case Some((f, c)) => (f.trim, Some(parse(c)))
        case None         => (rhsText.trim, None)
      IsOfForm(
        ExprParser.parse(lhsRaw),
        ExprParser.parseUntaggedForm(formRaw),
        condOpt,
        negated,
      )

    def parseRhs(lhsRaw: String, rhsRaw: String, negated: Boolean): Cond =
      rhsRaw.trim match
        case "missing"         => IsMissing(ExprParser.parse(lhsRaw), negated)
        case "given"           => IsMissing(ExprParser.parse(lhsRaw), !negated)
        case IsOfFormRhs(text) => parseIsOfForm(lhsRaw.trim, text, negated)
        case ContainedIn(listRaw) =>
          Contains(
            ExprParser.parse(lhsRaw),
            ExprParser.parse(listRaw),
            negated,
          )
        case ExposedIn(realmRaw) =>
          Exposed(ExprParser.parse(lhsRaw), ExprParser.parse(realmRaw), negated)
        case ArticleLink(noun) =>
          IsType(ExprParser.parse(lhsRaw), noun, negated)
        case ValidTypeLink(typeName) =>
          Eq(
            AlgoCall(s"valid_$typeName", List(ExprParser.parse(lhsRaw))),
            Bool(true),
            negated,
          )
        case _ =>
          Eq(
            ExprParser.parse(lhsRaw),
            ExprParser.parse(rhsRaw),
            negated,
          )

    // "is not equal to"/"does not equal" (and their positive counterparts)
    // are synonyms with the same handler — matched together via
    // findTopLevelAny, the same synonym-list idiom BinOpSeps/CompareOps use,
    // rather than two separately-duplicated `.orElse` stages. Order matters
    // here: each synonym pair must be tried before the shorter separator
    // it's a superstring of (" is not equal to " before " is not ", " is
    // equal to " before " is ") — see ExprParserSpec/CondParserSpec's
    // "order:"-tagged tests, which pin exactly this.
    def splitEq(seps: Seq[String]): Option[(String, String)] =
      findTopLevelAny(s, seps).map {
        case (i, sep) => (s.substring(0, i), s.substring(i + sep.length))
      }

    splitEq(
      Seq(" is not equal to ", " does not equal ", " is not the same as "),
    )
      .map {
        case (l, r) =>
          Eq(ExprParser.parse(l), ExprParser.parse(r), negated = true)
      }
      .orElse(
        splitEq(Seq(" is equal to ", " equals ", " is the same as ")).map {
          case (l, r) => Eq(ExprParser.parse(l), ExprParser.parse(r))
        },
      )
      .orElse(splitTopLevel(s, " is not ").map {
        case (l, r) => parseRhs(l, r, negated = true)
      })
      .orElse(
        splitTopLevel(s, " is ")
          .filter { case (_, r) => !r.trim.startsWith("one of") }
          .map { case (l, r) => parseRhs(l, r, negated = false) },
      )
      .orElse(findTopLevelAny(s, CompareOpSeps).map {
        case (i, op) =>
          Compare(
            ExprParser.parse(s.substring(0, i)),
            NormalizeOp(op),
            ExprParser.parse(s.substring(i + op.length)),
          )
      })
      .getOrElse(s match
        case IsTypeNeg(exprRaw, t) =>
          IsType(ExprParser.parse(exprRaw), t.trim, negated = true)
        case IsTypePos(exprRaw, t) =>
          IsType(ExprParser.parse(exprRaw), t.trim)
        case MatchesNeg(l, link, r) =>
          Matches(
            ExprParser.parse(l),
            matchType(link),
            ExprParser.parse(r),
            negated = true,
          )
        case MatchesPos(l, link, r) =>
          Matches(
            ExprParser.parse(l),
            matchType(link),
            ExprParser.parse(r),
          )
        case _ => Cond.Unknown(s),
      )
