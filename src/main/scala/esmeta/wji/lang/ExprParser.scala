package esmeta.wji.lang

import Expr.*
import TextSplit.*

/** Parses raw spec prose strings into [[Expr]] trees. */
object ExprParser:

  private val AbruptPrefix = """(?s)^\[=([?!])=\]\s+(.+)$""".r
  private val ResultOf = """(?si)^the result of (?:creating\s+)?(.+)$""".r
  private val EitherPat = """(?si)^either\s+(.+)$""".r
  private val JSCallFull = """(?s)^\[\$([^\$]+)\$\]\((.*)\)$""".r
  private val AlgoCallFull = """(?s)^(\[=[^\]]+\])\s*\((.*)\)$""".r
  private val AlgoCallProse = """(?s)^(\[=[^\]]+\])\s+(.+)$""".r
  private val AlgoLinkOnly = """(?s)^(\[=[^\]]+\])$""".r
  private val VarOnly = """(?s)^\|([^|]+)\|$""".r
  private val VarIgnore = """(?s)^<var\s+ignore>([^<]*)</var>$""".r
  private val ThisOnly = """(?s)^\*\*this\*\*$""".r
  private val NewExpr =
    """(?si)^a\s+\[=/new=\]\s+\{\{([^}]+)\}\}(?:\s+object)?$""".r
  private val EmptyList = """(?si)^a\s+new,?\s+empty\s+(?:\[=list=\]|list)$""".r
  // "a [=X=] which ..." — a relative-clause description of X, not a call
  // (e.g. "a [=Data Block=] which is [=identified with=] the underlying
  // memory of |memaddr|"). Not yet evaluable (see Expr.Described); matched
  // explicitly (rather than left to fall through to the default `Unknown`
  // case) so it can never be mistaken for AlgoCallIndefVar below.
  private val RelativeClauseDesc =
    """(?si)^an?\s+(\[=[^\]]+\])\s+which\s+(.+)$""".r
  // "(a|an|the) <desc> such that <cond>" — any definite/indefinite/superlative
  // description satisfying a predicate, not a call. Covers all the variants
  // seen in the spec: "a [=host address=] |hostaddr| exists such that ...",
  // "the unsigned integer such that |i64| is [=signed_64=](|u64|)", "an
  // implementation-defined integer such that ...", "the smallest address
  // such that ...". `desc` (non-greedy up to the first "such that") may or
  // may not itself contain a `[=link=]`/`|var|`/qualifier word like "exists"
  // or "smallest" — kept as raw text since the phrasing varies too much to
  // structure further. Not yet evaluable (see Expr.SuchThat); matched
  // explicitly for the same reason as RelativeClauseDesc above.
  private val SuchThatDesc =
    """(?si)^(?:the|an?)\s+(.+?)\s+such\s+that\s+(.+)$""".r
  // "a [=algo|display text=] (of)? |arg|" — a single-argument algorithm
  // invocation phrased as a noun (e.g. "a [=get a copy of the buffer
  // source|copy of the bytes held by the buffer=] |bytes|"), as opposed to
  // AlgoCallProse's "[=algo=] ARGS" verb phrasing. Deliberately anchored to a
  // single *bare variable* argument with nothing else trailing, so phrases
  // like RelativeClauseDesc/SuchThatDesc above (which have more text after
  // the bracket/variable) can't match here even if the guards above it were
  // ever removed. Placed after the more specific "a [=/new=] ..." / "a new,
  // empty ..." patterns above so it only catches the general case.
  private val AlgoCallIndefVar =
    """(?si)^an?\s+(\[=[^\]]+\])\s+(?:of\s+)?(\|[^|]+\|)$""".r
  private val PlainNewExpr = """(?si)^a\s+new\s+.+""".r
  private val SlotAccess = """(?s)^(.+)\.\\?\[\[([^\]]+)\]\]$""".r
  private val PossessiveSlot =
    """(?si)^the value of (.+)'s \\?\[\[([^\]]+)\]\] internal slot$""".r
  // e.g. "|module|.[=imports=]" — a WebAssembly-spec record field written
  // with a dot, where the `[=...=]` is a documentation link on the field
  // name rather than a call (contrast with `AlgoCallFull`/`AlgoCallProse`,
  // which require the string to *start* with `[=`).
  private val DotFieldLink = """(?s)^(.+)\.(\[=[^\]]+\])$""".r
  private val LengthOf =
    """(?si)^the (?:\[=(?:string/length|list/size)=\]|length) of (.+)$""".r
  private val ElementCount = """(?si)^the number of elements in (.+)$""".r
  private val ElementAt =
    """(?si)^the value of the element stored at index (.+) in (.+)$""".r
  private val AsMathPat =
    """(?si)^(.+)\s+interpreted as a \[=mathematical value=\]$""".r
  private val PowPat = """(?s)^(\d+)<sup>(.+?)</sup>$""".r
  private val BinOpPat =
    """(?s)^(.+)\s+(modulo|\+|-|\*|&div;|&minus;)\s+(.+)$""".r
  private val TuplePat = """(?s)^\((.+)\)$""".r
  private val NegPat = """(?s)^[-−](.+)$""".r
  private val PossessiveSize = """(?si)^(.+)'s \[=list/size=\]$""".r
  private val PossessiveAssociation =
    """(?si)^the (.+)'s (?:associated )?(\[=[^\]]+\])$""".r
  private val IndexByStr = """(?s)^(.+)\["([^"]+)"\]$""".r
  private val IndexByVar = """(?s)^(.+)\[(\|[^|]+\|)\]$""".r
  private val IndexByNum = """(?s)^(.+)\[(-?\d+)\]$""".r

  private val MapLiteral = """(?s)^«\[\s*(.*?)\s*\]»$""".r
  // spec error #1 (spec_errors.md): written as «  » instead of «[ ]»; SpecPatch corrects it,
  // but we match both forms for robustness
  private val EmptyMapProse = """(?si)^the ordered map «(?:\[\s*\])?\s*»$""".r
  private val ListLiteral = """(?s)^«\s*(.*?)\s*»$""".r
  private val NumberPat = """^\d+(?:\.\d+)?$""".r
  private val HexPat = """^0x[0-9a-fA-F]+$""".r
  private val QuotedStr = """^"([^"]*)"$""".r
  private val BoolTrue = """(?i)^true$""".r
  private val BoolFalse = """(?i)^false$""".r
  private val BoldConst = """(?s)^\*\*([^*]+)\*\*$""".r
  private val EmptyString = """(?i)^the empty string$""".r
  private val SpecTermPat = """(?i)^(?:undefined|null|empty|absent)$""".r
  private val EmuConst = """(?s)^(<emu-const>[^<]*</emu-const>)$""".r

  def normalizeLink(link: String): String =
    link.replaceAll("""\|[^=\]]*(?==\])""", "")

  /** Strips a Bikeshed `{{...}}` IDL-reference wrapper, e.g. a `[[...]]`
    * internal slot named after an intrinsic is conventionally written
    * `[[{{%Promise%}}]]` rather than `[[%Promise%]]`.
    */
  private def stripBraces(s: String): String =
    s.stripPrefix("{{").stripSuffix("}}")

  def parse(raw: String): Expr =
    val s = raw.trim
    s match
      case AbruptPrefix(check, rest) => Abrupt(check, parse(rest))
      case ResultOf(rest)            => parse(rest)
      case EitherPat(rest)           => parse(rest)
      case JSCallFull(name, argsRaw) =>
        JSCall(name, splitComma(argsRaw).map(parse))
      case AlgoCallFull(link, argsRaw) =>
        AlgoCall(normalizeLink(link), splitComma(argsRaw).map(parse))
      case AlgoCallProse(link, prose) =>
        AlgoCall(normalizeLink(link), parseArgs(prose))
      case AlgoLinkOnly(link)        => AlgoCall(normalizeLink(link), Nil)
      case ThisOnly()                => This
      case VarOnly(name)             => Var(name)
      case VarIgnore(name)           => Var(name.trim)
      case SlotAccess(baseRaw, slot) => Field(parse(baseRaw), stripBraces(slot))
      case PossessiveSlot(baseRaw, slot) =>
        Field(parse(baseRaw), stripBraces(slot))
      case DotFieldLink(baseRaw, link) =>
        Field(
          parse(baseRaw),
          normalizeLink(link).stripPrefix("[=").stripSuffix("=]"),
        )
      case LengthOf(inner)       => Length(parse(inner.trim))
      case ElementCount(inner)   => Length(parse(inner.trim))
      case ElementAt(idx, arr)   => Index(parse(arr.trim), parse(idx.trim))
      case PossessiveSize(inner) => Length(parse(inner.trim))
      case PossessiveAssociation(baseRaw, link) =>
        Field(
          parse(baseRaw),
          normalizeLink(link).stripPrefix("[=").stripSuffix("=]"),
        )
      case IndexByStr(baseRaw, key)    => Index(parse(baseRaw), Str(key))
      case IndexByVar(baseRaw, varRaw) => Index(parse(baseRaw), parse(varRaw))
      case IndexByNum(baseRaw, n)      => Index(parse(baseRaw), parse(n))
      case NewExpr(iface)              => New(iface)
      case EmptyList()                 => List_(Nil)
      case RelativeClauseDesc(link, desc) =>
        Described(normalizeLink(link), desc.trim)
      case SuchThatDesc(desc, cond) =>
        SuchThat(desc.trim, cond.trim)
      case AlgoCallIndefVar(link, arg) =>
        AlgoCall(normalizeLink(link), List(parse(arg)))
      case AsMathPat(inner)       => AsMath(parse(inner))
      case PowPat(base, exp)      => Pow(parse(base), parse(exp))
      case BinOpPat(lhs, op, rhs) => BinOp(parse(lhs), op, parse(rhs))
      case PlainNewExpr()         => UnknownNew(s)
      case EmptyMapProse()        => Map_(Nil)
      case MapLiteral(inner) =>
        val entries = splitComma(inner).map { e =>
          splitTopLevel(e, " → ") match
            case Some((k, v)) => (parse(k.trim), parse(v.trim))
            case None         => (Unknown(e), Unknown(""))
        }
        Map_(entries)
      case ListLiteral(inner) =>
        List_(splitComma(inner).map(parse))
      case TuplePat(inner) => Tuple(splitComma(inner).map(parse))
      case NegPat(inner)   => Neg(parse(inner))
      case NumberPat()     => Num(s)
      case HexPat()        => Num(s)
      case QuotedStr(v)    => Str(v)
      case EmptyString()   => Str("")
      case BoolTrue()      => Bool(true)
      case BoolFalse()     => Bool(false)
      case BoldConst(_)    => SpecTerm(s)
      case SpecTermPat()   => SpecTerm(s)
      case EmuConst(v)     => SpecTerm(v)
      case _               => Unknown(s)

  /** Extracts argument [[Expr]]s from a prose string.
    *
    * Start positions are restricted to word boundaries (position 0 and every
    * position right after a space). End positions shrink one character at a
    * time so trailing punctuation is trimmed naturally without pre-processing.
    */
  private[wji] def parseArgs(prose: String): List[Expr] =
    val s = prose.trim
    val n = s.length
    val starts = (0 until n).filter(i => i == 0 || s(i - 1) == ' ').toArray
    val result = collection.mutable.ListBuffer[Expr]()
    var si = 0
    while si < starts.length do
      val from = starts(si)
      var to = n
      var found = false
      while to > from && !found do
        parse(s.substring(from, to)) match
          case Unknown(_) => to -= 1
          case expr =>
            result += expr
            si = starts.indexWhere(_ >= to)
            if si < 0 then si = starts.length
            found = true
      if !found then si += 1
    result.toList
