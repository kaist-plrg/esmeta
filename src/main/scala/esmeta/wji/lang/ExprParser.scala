package esmeta.wji.lang

import Expr.*
import TextSplit.*

/** Parses raw spec prose strings into [[Expr]] trees. */
object ExprParser:

  private val AbruptPrefix = """(?s)^\[=([?!])=\]\s+(.+)$""".r
  private val ResultOf = """(?si)^the result of (?:creating\s+)?(.+)$""".r
  private val EitherPat = """(?si)^either\s+(.+)$""".r
  // "the [=TERM=] EXPR" — TERM names EXPR's type/category (e.g. "the
  // external value [=external value/func=] |funcaddr|" ~ "the (external
  // value) func funcaddr", mirroring how "the number 5"/"the list «1, 2»"
  // name a value's type before the value itself). Safe to drop TERM and
  // parse only EXPR: EXPR's own dfn/tag already carries that information on
  // its own (see Link/Case), so nothing is lost. General over TERM — not
  // specific to any one dfn. EXPR is either another `[=...=]` link (the
  // common case above) or a `|var|` reference chain with more after it (e.g.
  // "the [=memory address=] |frame|.[=frame/module=]...[|x|]") — the `.+`
  // after the closing `|` requires at least one more character, so a *bare*
  // "the [=TERM=] |var|" (nothing trailing) still falls through to
  // LinkIndefVar below, where TERM is the call and |var| is its argument
  // rather than droppable annotation.
  private val TypeAnnotatedPrefix =
    """(?si)^the\s+\[=[^\]]+=\]\s+(\[=.+|\|[^|]+\|.+)$""".r
  // "the {{TERM}} value" — the `{{...}}` (Bikeshed/WebIDL autolink) form of
  // the TERM annotation above, e.g. "the {{undefined}} value" (bare — the
  // value *is* the named term, so it parses straight to a SpecTerm) or "the
  // {{WebAssemblyInstantiatedSource}} value «[ ... ]»" (a payload follows —
  // TERM only annotates the payload's type, so it's dropped and the payload
  // parsed on its own, same idiom as TypeAnnotatedPrefix). The prefix form
  // (with payload) is tried first since it's the more specific match.
  private val BracedTermValuePrefix =
    """(?si)^the\s+\{\{[^}]+\}\}\s+value\s+(.+)$""".r
  private val BracedTermValueOnly =
    """(?si)^the\s+\{\{([^}]+)\}\}\s+value$""".r
  // Three phrasings of the spec's "the following steps ...:" closure idiom —
  // all parse straight to a FollowingSteps placeholder, later hoisted into a
  // real Closure by ExpandFollowingStepsPass (the substeps themselves stay on
  // the owning instruction's `body` until then).
  // "the following steps given argument(s) |V|[, |W|, ...]:"
  private val StepsClosurePrefix =
    """(?is)^the following steps,?\s+given\s+(?:arguments?\s+)?(\|[^|]+\|(?:\s*(?:,|and)\s*\|[^|]+\|)*)\s*:?\s*$""".r
  // "[if provided,] to perform the following steps:" — a `Queue a task`
  // step's closure clause; no params (mirrors a Job Abstract Closure).
  private val QueueTaskClosureSuffix =
    """(?is)^(?:if\s+provided,\s*)?to\s+perform\s+the\s+following\s+steps:?\s*$""".r
  // "a [=term=] which performs the following steps when called with
  // argument(s) |V|[, ...]:" — must precede RelativeClauseDesc, which would
  // otherwise swallow it into an unevaluable Described.
  private val WhichPerformsStepsClosure =
    """(?si)^an?\s+\[=[^\]]+=\]\s+which\s+performs\s+the\s+following\s+steps\s+when\s+called\s+with\s+(?:arguments?\s+)?(\|[^|]+\|(?:\s*(?:,|and)\s*\|[^|]+\|)*)\s*:?\s*$""".r
  // "performing CLOSURE[,] given ARG[, ARG...]" — invoking a closure *value*
  // (contrast with the three "the following steps ...:" forms above, which
  // *define* one), e.g. "the result of performing |onFullfilledStepsArg|
  // given |value|" (patched PromiseReactionJob text, see SpecPatch). CLOSURE
  // is parsed as a general Expr (not just a bare |var|) since nothing about
  // the phrasing restricts it to a variable reference.
  private val PerformingClosureCall = """(?si)^performing\s+(.+)$""".r
  private val PipeVarInline = """\|([^|]+)\|""".r
  private val Backticked = """(?s)^`(.+)`$""".r
  private val BacktickedQuotedStr = """(?s)^`"([^"]*)"`$""".r
  private val JSCallFull = """(?s)^\[\$([^\$]+)\$\]\((.*)\)$""".r
  private val LinkFull = """(?s)^(\[=[^\]]+\])\s*\((.*)\)$""".r
  private val LinkProse = """(?s)^(\[=[^\]]+\])\s+(.+)$""".r
  private val LinkOnly = """(?s)^(?:the\s+)?(\[=[^\]]+\])$""".r
  private val VarOnly = """(?s)^\|([^|]+)\|$""".r
  private val VarIgnore = """(?s)^<var\s+ignore>([^<]*)</var>$""".r
  private val ThisOnly = """(?s)^\*\*this\*\*$""".r
  private val NewExpr =
    """(?si)^a\s+\[=/new=\]\s+\{\{([^}]+)\}\}(?:\s+object)?$""".r
  // "a {{X}} exception" — Bikeshed's common idiom for constructing a new
  // exception object of WebIDL/spec type X (e.g. "throw a {{TypeError}}
  // exception", "reject |promise| with a {{CompileError}} exception").
  // Semantically the same "freshly constructed instance of interface X" as
  // NewExpr's "a [=/new=] {{X}}" form above, so it reuses the same New(iface)
  // node rather than adding a new one.
  private val NewExceptionExpr =
    """(?si)^an?\s+\{\{([^}]+)\}\}\s+exception$""".r
  private val EmptyList = """(?si)^a\s+new,?\s+empty\s+(?:\[=list=\]|list)$""".r
  // "a new [=byte sequence=] of [=byte sequence/length=] equal to LENGTH" — a
  // freshly allocated all-zero byte sequence of the given length.
  private val NewByteSeqOfLength =
    """(?si)^a\s+new\s+\[=byte sequence=\]\s+of\s+\[=byte sequence/length=\]\s+equal to\s+(.+)$""".r
  // "a [=X=] which ..." — a relative-clause description of X, not a call
  // (e.g. "a [=Data Block=] which is [=identified with=] the underlying
  // memory of |memaddr|"). Not yet evaluable (see Expr.Described); matched
  // explicitly (rather than left to fall through to the default `Unknown`
  // case) so it can never be mistaken for LinkIndefVar below.
  private val RelativeClauseDesc =
    """(?si)^an?\s+(\[=[^\]]+\])\s+which\s+(.+)$""".r
  // "of type <code>...&lt;X&gt;...</code>" — Bikeshed's convention for
  // instantiating a generic operation's declared type parameter at a call
  // site (mirrors AlgorithmExtractor's generic-bracket `<var ignore>`/`|T|`
  // detection on the definition side — see
  // AlgorithmExtractor.GenericVarIgnore). X, the innermost generic argument,
  // is a symbolic type tag rather than a computed runtime value, so it
  // parses to a bare SpecTerm like any other glossary/interface-name
  // reference (e.g. `current Realm`). Tolerates a literal `>` as well as
  // `&gt;` for the closing bracket — the real spec source writes it both
  // ways (contrast webidl's `a new promise`/`get a promise for waiting for
  // all`). Only handles a single (non-nested) generic argument — a known
  // gap, same spirit as other narrowly-scoped rules in this file.
  private val OfTypeGeneric =
    """(?si)^of\s+type\s+<code>.*?&lt;\s*(?:<a\b[^>]*>)?([A-Za-z][A-Za-z0-9]*)(?:</a>)?\s*(?:&gt;|>)\s*</code>$""".r
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
  // LinkProse's "[=algo=] ARGS" verb phrasing. Deliberately anchored to a
  // single *bare variable* argument with nothing else trailing, so phrases
  // like RelativeClauseDesc/SuchThatDesc above (which have more text after
  // the bracket/variable) can't match here even if the guards above it were
  // ever removed. Placed after the more specific "a [=/new=] ..." / "a new,
  // empty ..." patterns above so it only catches the general case.
  private val LinkIndefVar =
    """(?si)^(?:the|an?)\s+(\[=[^\]]+\])\s+(?:of\s+)?(\|[^|]+\|)$""".r
  private val PlainNewExpr = """(?si)^a\s+new\s+.+""".r
  private val SlotAccess = """(?s)^(.+)\.\\?\[\[([^\]]+)\]\]$""".r
  private val PossessiveSlot =
    """(?si)^the value of (.+)'s \\?\[\[([^\]]+)\]\] internal slot$""".r
  // e.g. "|module|.[=imports=]" — a WebAssembly-spec record field written
  // with a dot, where the `[=...=]` is a documentation link on the field
  // name rather than a call (contrast with `LinkFull`/`LinkProse`,
  // which require the string to *start* with `[=`).
  private val DotFieldLink = """(?s)^(.+)\.(\[=[^\]]+\])$""".r
  // "VALUE, [=link=]" — spec's passive-voice idiom for a unary conversion
  // applied to the value stated just before it (e.g. "|result|,
  // [=converted to a JavaScript value=]", "|map|'s [=map/size=], [=converted
  // to a JavaScript value=]." — both from webidl/index.bs). Mirrors
  // DotFieldLink's `base.[=name=]` shape but with a comma rather than a dot;
  // unlike DotFieldLink (a field read), this is a call, so it produces a
  // Link (resolved to AlgoCall/Case by ResolveLinksPass), not Field.
  private val TrailingLinkCall = """(?s)^(.+),\s*(\[=[^\]]+\])$""".r
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
  private def parseBOp(op: String): BOp = op match
    case "+"             => BOp.Add
    case "-" | "&minus;" => BOp.Sub
    case "*"             => BOp.Mul
    case "&div;"         => BOp.Div
    case "modulo"        => BOp.Mod
  private val TuplePat = """(?s)^\((.+)\)$""".r
  private val NegPat = """(?s)^[-−](.+)$""".r
  private val PossessiveSize = """(?si)^(.+)'s \[=list/size=\]$""".r
  private val PossessiveAssociation =
    """(?si)^the (.+)'s (?:associated )?(\[=[^\]]+\])$""".r
  // "|func|'s [=associated Realm=]" — narrower than PossessiveAssociation
  // (which keeps "the surrounding agent's associated store/cache" style
  // field names as literal WJI-only state, and requires a leading "the").
  // webidl/index.bs's own "associated realm" dfn defines this, for the
  // common case (a non-exotic function object — not a callable proxy, not a
  // bound function), as *equal to* the object's real ECMA-262 [[Realm]]
  // internal slot, so this reads straight through to that slot rather than
  // a made-up "associated realm" field nothing else produces or consumes.
  // Bound functions / callable proxies aren't handled (webidl/index.bs
  // itself calls the general mechanism "underspecified"); revisit if one
  // is ever passed here as `func`.
  private val AssociatedRealm = """(?si)^(.+)'s \[=associated Realm=\]$""".r
  // "[|parameters|] → [|results|]" — SpecTec's comptype arrow notation for a
  // functype (`al_of_comptype`'s `FuncT (rt1, rt2) -> CaseV ("->", [rt1;
  // rt2])`): a params-list and a results-list side by side, each written
  // wrapped in its own `[...]` (decorating it as list-shaped, not a nested
  // destructure — `rt1`/`rt2` are each already an AL list value in their own
  // right, so the *whole* bracket content names one variable bound to that
  // whole list, mirroring `Cond.IsOfForm`'s `Expr.Case` args). Placed before
  // IndexByStr/IndexByVar/etc below — those would otherwise misparse this as
  // `base[key]` indexing, since the text happens to end in `[...]` too.
  // Requires non-empty content on both sides (a bare `[]` side, e.g. "Let
  // [|types|] → [] be ...", isn't reached yet — left as a gap to extend
  // into if/when it is).
  private val CompTypeArrow =
    """(?s)^\[\s*([^\[\]]+)\s*\]\s*→\s*\[\s*([^\[\]]+)\s*\]$""".r
  private val IndexByStr = """(?s)^(.+)\["([^"]+)"\]$""".r
  private val IndexByVar = """(?s)^(.+)\[(\|[^|]+\|)\]$""".r
  private val IndexByNum = """(?s)^(.+)\[(-?\d+)\]$""".r
  // a general `base[EXPR]` index whose key is a compound expression rather than
  // a bare string/var/number (e.g. `|bytes|[|i| &minus; |offset|]`). The key
  // holds no further brackets, so the whole bracketed suffix stays together and
  // isn't split by a `&minus;`/`+` inside it (which BinOpPat, tried later, is
  // not bracket-aware about). The base must end in a non-space char (index
  // syntax is written `base[key]` with no gap, so a space-then-`[` like the
  // `→ [...]` in a func-type destructuring is not an index) and the key must
  // not start with `=` (so a trailing `[=link=]` documentation link such as
  // `|func|'s [=associated Realm=]` is not mistaken for an index). Placed after
  // the specific IndexBy* forms above.
  private val IndexByExpr = """(?s)^(.+\S)\[([^\[\]=][^\[\]]*)\]$""".r

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
  // the value bound by a preceding `Cond.Throws` check ("If this throws an
  // exception, catch it, ... with the exception, ..."); "catch it" itself
  // carries no separate binding, so this is the only place that name needs
  // to resolve to a variable.
  private val TheException = """(?i)^the exception$""".r
  private val SpecTermPat = """(?i)^(?:undefined|null|empty|absent)$""".r
  // captures just the inner text (e.g. "throw"/"normal") — the enum value a
  // real completion record's [[Type]] actually holds, not the markup itself.
  private val EmuConst = """(?s)^<emu-const>([^<]*)</emu-const>$""".r
  // Unlike `<emu-const>` (an opaque spec-constant name, kept tag-and-all as
  // the SpecTerm), `<emu-val>` wraps an actual JS literal (`undefined`,
  // `null`, `true`, `false`) — only the inner text is captured, so
  // `<emu-val>undefined</emu-val>` becomes `SpecTerm("undefined")` and
  // unifies with the existing bare-`undefined`/`null` cases in the compiler
  // instead of falling through to a bogus EEnum.
  private val EmuVal = """(?s)^<emu-val>([^<]*)</emu-val>$""".r
  // A bare Bikeshed/WebIDL `{{...}}` autolink used as a value — a WebIDL type
  // or enumeration reference (e.g. `{{uint8}}`, `{{unordered}}`,
  // `{{undefined}}`, `{{%Symbol.iterator%}}`). The braces are a link marker,
  // not part of the name, so they're stripped to a bare SpecTerm — this lets
  // `{{undefined}}` unify with the `null`/`undefined` SpecTerms the compiler
  // already special-cases. Structural `{{...}}` uses (a `[=/new=] {{Iface}}`
  // object, a `[[{{%Promise%}}]]` slot) are matched by more specific patterns
  // (NewExpr, SlotAccess) before this.
  private val BracedTerm = """(?s)^\{\{(.+)\}\}$""".r
  // "the <a spec=HTML>incumbent settings object</a>" — a cross-spec Bikeshed
  // autolink (`<a spec=X>...</a>`) referencing a concept defined in another
  // spec entirely. ECMA-262 itself deliberately never defines "settings
  // object"/"incumbent settings object" — it delegates all of it to
  // HostMakeJobCallback/HostCallJobCallback (host-defined abstract
  // operations), whose *default* implementation (used by any host that isn't
  // a web browser — ecma262/spec.html's own wording) just calls the callback
  // directly with no such bookkeeping at all. So, like `current Realm`/
  // `surrounding agent`, this parses to an opaque SpecTerm placeholder —
  // nothing in this codebase ever needs its actual value, only that the
  // binding succeeds.
  private val CrossSpecRef = """(?si)^(?:the\s+)?<a\s+spec=\w+>(.+?)</a>$""".r
  // "|realm|'s [=realm/settings object=]" — the same WHATWG HTML machinery as
  // CrossSpecRef above, just accessed via a possessive rather than a direct
  // `<a spec=...>` link; the |realm| association is dropped rather than
  // modeled as a real field read, for the same reason.
  private val RealmSettingsObject =
    """(?si)^\|[^|]+\|'s \[=realm/settings object=\]$""".r
  // "[=the range=] LOW to HIGH" — the leading link text varies ("the range",
  // "range", ...) but always mentions "range". Both bounds are assumed
  // inclusive (see [[Expr.Range]]), so a trailing ", inclusive" is left for
  // the caller to strip along with the rest of the sentence.
  private val RangePrefix = """(?is)^\[=[^=]*range[^=]*=\]\s+(.+)$""".r

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
      case AbruptPrefix(check, rest)   => Abrupt(check, parse(rest))
      case ResultOf(rest)              => parse(rest)
      case EitherPat(rest)             => parse(rest)
      case TypeAnnotatedPrefix(rest)   => parse(rest)
      case BracedTermValuePrefix(rest) => parse(rest)
      case BracedTermValueOnly(term)   => SpecTerm(term)
      case StepsClosurePrefix(paramsRaw) =>
        FollowingSteps(
          PipeVarInline.findAllMatchIn(paramsRaw).map(_.group(1)).toList,
        )
      case QueueTaskClosureSuffix() => FollowingSteps(Nil)
      case WhichPerformsStepsClosure(paramsRaw) =>
        FollowingSteps(
          PipeVarInline.findAllMatchIn(paramsRaw).map(_.group(1)).toList,
        )
      case PerformingClosureCall(rest)
          if findTopLevel(rest, " given ").isDefined =>
        val (closureRaw, argsRaw) = splitTopLevel(rest, " given ").get
        ClosureCall(
          parse(closureRaw.stripSuffix(",").trim),
          splitComma(argsRaw.trim.replaceFirst("""(?i)^arguments?\s+""", ""))
            .map(parse),
        )
      // A backtick-wrapped *quoted* string (e.g. `"frozen"`, the argument to
      // [$SetIntegrityLevel$]) isn't a real ECMAScript string value — it's
      // Bikeshed's way of typesetting an ECMA-262 "specification type" enum
      // constant (mirroring how `~frozen~` reads in ecmarkup), so it parses
      // as a SpecTerm (-> `ir.EEnum`), same as any other bare spec constant,
      // not as a `Str` (-> `ir.EStr`). Plain backtick-wrapped code (no inner
      // quotes) carries no meaning of its own; strip it and re-parse.
      case BacktickedQuotedStr(v) => SpecTerm(v)
      case Backticked(inner)      => parse(inner)
      case RangePrefix(rest) if findTopLevel(rest, " to ").isDefined =>
        val i = findTopLevel(rest, " to ").get
        Range(parse(rest.substring(0, i)), parse(rest.substring(i + 4)))
      case JSCallFull(name, argsRaw) =>
        JSCall(name, splitComma(argsRaw).map(parse))
      // unlike LinkProse/LinkOnly below, the explicit `(...)` here is
      // unambiguous call syntax (mirrors JSCallFull) — no term/value
      // reference is ever written this way — so this can go straight to
      // AlgoCall without waiting for ResolveLinksPass.
      case LinkFull(link, argsRaw) =>
        AlgoCall(normalizeLink(link), splitComma(argsRaw).map(parse))
      case LinkProse(link, prose) =>
        Link(normalizeLink(link), parseArgs(prose))
      case LinkOnly(link)            => Link(normalizeLink(link), Nil)
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
      case TrailingLinkCall(valueRaw, link) =>
        Link(normalizeLink(link), List(parse(valueRaw.trim)))
      case LengthOf(inner)          => Length(parse(inner.trim))
      case ElementCount(inner)      => Length(parse(inner.trim))
      case ElementAt(idx, arr)      => Index(parse(arr.trim), parse(idx.trim))
      case PossessiveSize(inner)    => Length(parse(inner.trim))
      case AssociatedRealm(baseRaw) => Field(parse(baseRaw.trim), "Realm")
      case PossessiveAssociation(baseRaw, link) =>
        Field(
          parse(baseRaw),
          normalizeLink(link).stripPrefix("[=").stripSuffix("=]"),
        )
      case CompTypeArrow(paramsRaw, resultsRaw) =>
        Case("->", List(parse(paramsRaw), parse(resultsRaw)))
      case IndexByStr(baseRaw, key)    => Index(parse(baseRaw), Str(key))
      case IndexByVar(baseRaw, varRaw) => Index(parse(baseRaw), parse(varRaw))
      case IndexByNum(baseRaw, n)      => Index(parse(baseRaw), parse(n))
      case IndexByExpr(baseRaw, idx)   => Index(parse(baseRaw), parse(idx))
      case NewExpr(iface)              => New(iface)
      case NewExceptionExpr(iface)     => New(iface)
      case EmptyList()                 => List_(Nil)
      case NewByteSeqOfLength(lenRaw)  => NewByteSequence(parse(lenRaw.trim))
      case RelativeClauseDesc(link, desc) =>
        Described(normalizeLink(link), desc.trim)
      case OfTypeGeneric(typeArg) => SpecTerm(typeArg)
      case SuchThatDesc(desc, cond) =>
        SuchThat(desc.trim, cond.trim)
      case LinkIndefVar(link, arg) =>
        Link(normalizeLink(link), List(parse(arg)))
      case AsMathPat(inner)       => AsMath(parse(inner))
      case PowPat(base, exp)      => Pow(parse(base), parse(exp))
      case BinOpPat(lhs, op, rhs) => BinOp(parse(lhs), parseBOp(op), parse(rhs))
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
      case TuplePat(inner)       => Tuple(splitComma(inner).map(parse))
      case NegPat(inner)         => Neg(parse(inner))
      case NumberPat()           => Num(s)
      case HexPat()              => Num(s)
      case QuotedStr(v)          => Str(v)
      case EmptyString()         => Str("")
      case TheException()        => Var("exception")
      case BoolTrue()            => Bool(true)
      case BoolFalse()           => Bool(false)
      case BoldConst(_)          => SpecTerm(s)
      case SpecTermPat()         => SpecTerm(s)
      case EmuConst(v)           => SpecTerm(v)
      case EmuVal(v)             => SpecTerm(v)
      case BracedTerm(inner)     => SpecTerm(inner)
      case CrossSpecRef(text)    => SpecTerm(text)
      case RealmSettingsObject() => SpecTerm("realm/settings object")
      case _                     => Unknown(s)

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
