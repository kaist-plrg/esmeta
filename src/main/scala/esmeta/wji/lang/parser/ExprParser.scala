package esmeta.wji.lang.parser

import esmeta.wji.lang.*

import Expr.*
import TextSplit.*

/** Parses raw spec prose strings into [[Expr]] trees.
  *
  * `parse`'s cases are grouped into roles (see the section headers below and
  * each pattern's own doc comment for which role it belongs to):
  *   - '''Wrappers''': strip an outer decoration/annotation and recurse into
  *     what's left (or, if there's no payload, terminate directly).
  *   - '''Closures''': the "the following steps ...:" definition idioms, and
  *     invoking a closure *value*.
  *   - '''Construction''': builds a new composite value — object, exception,
  *     list/map/byte-sequence, or range.
  *   - '''Call syntax''': explicit invocation of a named algorithm/AO.
  *   - '''Bare references''': `|var|`/`**this**` and nothing else.
  *   - '''Arithmetic & casts''': binary/unary operators and math-value casts.
  *   - '''Structural access''': reading a field/slot/index/length/association
  *     off a base value.
  *   - '''Noun-phrase descriptions''': indefinite/definite descriptions that
  *     are either not yet evaluable, or a narrowly-scoped single-arg call.
  *   - '''Scalars & glossary terms''': literal values and fixed spec-glossary
  *     constants.
  *
  * As with [[esmeta.wji.compiler.lowering.Lowering]]'s pass categories, this
  * grouping is documentation only: `parse` is one Scala `match`, so within a
  * role (and, where noted, across roles — e.g. arithmetic before structural
  * access) the *order* cases appear in still matters and is what's actually
  * checked by `ExprParserSpec`'s "order:"-tagged tests, not the role grouping
  * itself.
  */
object ExprParser:

  // ---- Wrappers ----

  private val AbruptPrefix = """(?s)^\[=([?!])=\]\s+(.+)$""".r
  // "CALL, where the destination type is associated with [=[ATTR]=]" —
  // webidl/index.bs's ConvertToInt (7604) reads its EnforceRange/Clamp
  // behavior from the *ambient* extended attributes of whatever IDL type
  // conversion it's inlined into, not from one of its own 3 formal
  // parameters (every real caller sits inside one of the "convert a
  // JavaScript value to X" algorithms, where that context is implicit).
  // AddressValueToU64 calls it directly, outside that machinery, so
  // js-api/index.bs spells the attribute out with this trailing clause.
  // Rather than dropping it (losing the information) or teaching a whole new
  // node just to carry it, folds it straight into the wrapped call's own arg
  // list as one more literal string — turning the ambient context into a
  // real, explicit argument `ConvertToInt`'s manual implementation can read
  // (see docs/hardcodes.md #13).
  private val AssociatedWithAttrPat =
    """(?si)^(.+),\s*where the destination type is associated with \[=\[(\w+)\]=\]$""".r
  private val ResultOf = """(?si)^the result of (?:creating\s+)?(.+)$""".r
  private val EitherPat = """(?si)^either\s+(.+)$""".r
  // "the [=TERM=] EXPR" — TERM names EXPR's type/category (e.g. "the
  // external value [=external value/func=] |funcaddr|" ~ "the (external
  // value) func funcaddr", mirroring how "the number 5"/"the list «1, 2»"
  // name a value's type before the value itself). Whether TERM is safe to
  // drop (EXPR's own dfn/tag already carries the same information — see
  // Link/Case) or is actually needed (EXPR ends up untagged, e.g. Seq_) isn't
  // decidable here — this parser sees one phrase at a time with no broader
  // SpecTec-grammar knowledge — so both TERM and EXPR are kept, uniformly, in
  // Expr.TypeAnnotated; see that node's own doc for who resolves it and when.
  // General over TERM — not specific to any one dfn. EXPR is either another
  // `[=...=]` link (the common case above) or a `|var|` reference chain with
  // more after it (e.g. "the [=memory address=]
  // |frame|.[=frame/module=]...[|x|]") — the `.+` after the closing `|`
  // requires at least one more character, so a *bare* "the [=TERM=] |var|"
  // (nothing trailing) still falls through to LinkIndefVar below, where TERM
  // is the call and |var| is its argument rather than an annotation.
  private val TypeAnnotatedPrefix =
    """(?si)^the\s+\[=((?:(?!=\]).)+?)=\]\s+(\[=.+|\|[^|]+\|.+)$""".r
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
  // A backtick-wrapped *quoted* string (e.g. `"frozen"`, the argument to
  // [$SetIntegrityLevel$]) isn't a real ECMAScript string value — it's
  // Bikeshed's way of typesetting an ECMA-262 "specification type" enum
  // constant (mirroring how `~frozen~` reads in ecmarkup), so it parses
  // as a SpecTerm (-> `ir.EEnum`), same as any other bare spec constant,
  // not as a `Str` (-> `ir.EStr`). Plain backtick-wrapped code (no inner
  // quotes) carries no meaning of its own; strip it and re-parse. Tried
  // before the plain form below since it's the more specific match.
  private val BacktickedQuotedStr = """(?s)^`"([^"]*)"`$""".r
  private val Backticked = """(?s)^`(.+)`$""".r

  // ---- Closures: "the following steps ...:" definitions, and invoking a
  // closure value ----

  // Four phrasings of the spec's "the following steps ...:" closure idiom —
  // all parse straight to a FollowingSteps placeholder, later hoisted into a
  // real Closure by ExpandFollowingStepsPass (the substeps themselves stay on
  // the owning instruction's `body` until then).
  // "the following steps given the list of arguments |V|:" — WJI-invented
  // phrasing (SpecPatch-authored, not real spec prose): a variadic-style
  // closure param binding the *entire* JS arguments list itself, not one
  // positional value (contrast StepsClosurePrefix just below). Single var
  // only — no real text needs more than one variadic param. Wording echoes
  // `call an Exported Function`'s own declared param ("a list of JavaScript
  // arguments |argValues|", index.bs:1279). Tried first since it's the more
  // specific of the two "given ...:" phrasings. See SpecPatch #22. Also
  // matches `creating an operation function`'s real-prose equivalent "the
  // following series of steps, given function argument values |args|:"
  // (index.bs:12541) — |args| there is likewise read wholesale as a list,
  // not destructured positionally.
  private val VariadicStepsClosurePrefix =
    """(?is)^the following (?:series of )?steps,?\s+given\s+(?:the\s+list\s+of\s+arguments|function\s+argument\s+values)\s+\|([^|]+)\|\s*:?\s*$""".r
  // "the following steps given argument(s) |V|[, |W|, ...]:"
  private val StepsClosurePrefix =
    """(?is)^the following steps,?\s+given\s+(?:arguments?\s+)?(\|[^|]+\|(?:\s*(?:,|and)\s*\|[^|]+\|)*)\s*:?\s*$""".r
  // "[if provided,] to perform the following steps:" — a `Queue a task`
  // step's closure clause; no params (mirrors a Job Abstract Closure).
  private val QueueTaskClosureSuffix =
    """(?is)^(?:if\s+provided,\s*)?to\s+perform\s+the\s+following\s+steps:?\s*$""".r
  // "the following steps:" / "the following series of steps:" — the same
  // closure-introduction idiom as StepsClosurePrefix/VariadicStepsClosurePrefix
  // above, but with no "given ..." parameter clause at all (e.g. attribute
  // getter/setter's own steps closure, webidl_yet_categorized.md category
  // I-H — neither reads a bound closure param, both read **this**/the
  // implicit setter value directly instead). Zero-arg, like
  // QueueTaskClosureSuffix just above, but this file's own "the following
  // steps"/"the following series of steps" wording rather than "to perform
  // the following steps".
  private val BareStepsClosure =
    """(?is)^the following (?:series of )?steps:?\s*$""".r
  // "a [=term=] which performs the following steps when called with
  // argument(s) |V|[, ...]:" — must precede RelativeClauseDesc, which would
  // otherwise swallow it into an unevaluable Described.
  private val WhichPerformsStepsClosure =
    """(?si)^an?\s+\[=(?:(?!=\]).)+?=\]\s+which\s+performs\s+the\s+following\s+steps\s+when\s+called\s+with\s+(?:arguments?\s+)?(\|[^|]+\|(?:\s*(?:,|and)\s*\|[^|]+\|)*)\s*:?\s*$""".r
  // "a [=term=] which performs the following steps when called with state
  // |S| and arguments |V|:" — SpecPatch-authored (`create a host function`,
  // #28), a leading named `state` parameter ahead of the usual variadic
  // `arguments` one. Tried before WhichPerformsStepsClosure above (more
  // specific — that pattern's leading `(?:arguments?\s+)?` only tolerates
  // the literal word "argument(s)" before the first `|var|`, not "state").
  private val WhichPerformsStepsClosureWithState =
    """(?si)^an?\s+\[=(?:(?!=\]).)+?=\]\s+which\s+performs\s+the\s+following\s+steps\s+when\s+called\s+with\s+state\s+\|([^|]+)\|\s+and\s+arguments\s+\|([^|]+)\|\s*:?\s*$""".r
  // "performing CLOSURE[,] given ARG[, ARG...]" — invoking a closure *value*
  // (contrast with the four "the following steps ...:" forms above, which
  // *define* one), e.g. "the result of performing |onFullfilledStepsArg|
  // given |value|" (patched PromiseReactionJob text, see SpecPatch). CLOSURE
  // is parsed as a general Expr (not just a bare |var|) since nothing about
  // the phrasing restricts it to a variable reference.
  private val PerformingClosureCall = """(?si)^performing\s+(.+)$""".r
  // "running CLOSURE[,] given ARG[, ARG...]" / "running CLOSURE" — WebIDL's
  // own verb for invoking a closure value or a freshly *defined* one
  // immediately (contrast PerformingClosureCall's "performing ...", used
  // elsewhere for the same idea) — e.g. "Try running the following steps:
  // ..." invokes a closure defined right there via one of the "the
  // following steps ...:" forms above (CLOSURE is parsed as a general Expr,
  // same as PerformingClosureCall, so this falls straight out of that).
  // Unlike PerformingClosureCall, also matches with no "given ..." clause at
  // all — that's the shape this idiom actually uses (no closure params).
  private val RunningClosureCall = """(?si)^running\s+(.+)$""".r
  private val PipeVarInline = """\|([^|]+)\|""".r
  // "the [=X steps=] for/of |BASE|[,] with |ARG1| as [=this=] and |ARG2| as
  // the argument values" — WebIDL's idiom for invoking an interface member's
  // own steps closure, either as a value ("the result of running the
  // [=default method steps=]/[=method steps=] ...", see RunningStepsCall
  // below) or as a statement ("Perform the [=constructor steps=] of ...",
  // see InstrParser's PerformStepsPrefix) — the verb is stripped by the
  // caller, so both share [[parseStepsCall]]. A setter's lone argument is
  // spelled "|ARG2| as [=the given value=]" instead ("Perform the [=setter
  // steps=] of |attribute|, with |idlObject| as [=this=] and |idlValue| as
  // [=the given value=]") — same positional shape, so it's passed the same
  // way (webidl_yet_categorized.md category III-B).
  private val StepsCallWithArgs =
    """(?si)^the \[=([\w\s]+?) steps=\]\s+(?:for|of)\s+(\|[^|]+\|),?\s+with\s+(\|[^|]+\|)\s+as\s+\[=this=\]\s+and\s+(\|[^|]+\|)\s+as\s+(?:the\s+argument\s+values|\[=the given value=\])$""".r
  // same idiom, no "and ... as the argument values" clause — WebIDL's "the
  // result of running the [=getter steps=] ... with ... as [=this=]".
  private val StepsCallNoArgs =
    """(?si)^the \[=([\w\s]+?) steps=\]\s+(?:for|of)\s+(\|[^|]+\|),?\s+with\s+(\|[^|]+\|)\s+as\s+\[=this=\]$""".r
  // "running STEPS-CALL" — the leading "the result of " is already stripped
  // by ResultOf by the time this is tried.
  private val RunningStepsCall = """(?si)^running\s+(the \[=.+)$""".r

  /** `rest` is everything after the verb ("running"/"Perform") of a
    * [[StepsCallWithArgs]]/[[StepsCallNoArgs]] steps-closure call; `None` if it
    * isn't one.
    */
  private[wji] def parseStepsCall(rest: String): Option[ClosureCall] =
    rest.trim match
      case StepsCallWithArgs(stepsRaw, baseRaw, thisArgRaw, argsVarRaw) =>
        Some(
          ClosureCall(
            Field(parse(baseRaw), stepsFieldName(stepsRaw)),
            List(parse(thisArgRaw), parse(argsVarRaw)),
          ),
        )
      case StepsCallNoArgs(stepsRaw, baseRaw, thisArgRaw) =>
        Some(
          ClosureCall(
            Field(parse(baseRaw), stepsFieldName(stepsRaw)),
            List(parse(thisArgRaw)),
          ),
        )
      case _ => None

  /** Shared by [[PerformingClosureCall]]/[[RunningClosureCall]]: `rest` is
    * everything after the verb — a closure (value or freshly-defined), with an
    * optional trailing `" given ARG[, ARG...]"` argument list.
    */
  private def closureCall(rest: String): Expr =
    splitTopLevel(rest, " given ") match
      case Some((closureRaw, argsRaw)) =>
        ClosureCall(
          parse(closureRaw.stripSuffix(",")),
          splitComma(argsRaw.trim.replaceFirst("""(?i)^arguments?\s+""", ""))
            .map(parse),
        )
      case None => ClosureCall(parse(rest), Nil)

  // ---- Construction: builds a new composite value ----

  // "[=the range=] LOW to HIGH" — the leading link text varies ("the range",
  // "range", ...) but always mentions "range". Both bounds are assumed
  // inclusive (see [[Expr.Range]]), so a trailing ", inclusive" is left for
  // the caller to strip along with the rest of the sentence. Must precede
  // LinkProse below, which would otherwise misparse this as a call.
  private val RangePrefix = """(?is)^\[=[^=]*range[^=]*=\]\s+(.+)$""".r
  // "a [=/new=] {{X}} in the [=REALM=]" — webidl/index.bs's "new" op (line
  // 13818, `create a new object implementing the interface`) declares a
  // *required* |realm| parameter alongside |interface|, but every js-api
  // call site originally omitted it outright (docs/spec_errors.md #22) —
  // SpecPatch-corrected to append this realm clause, mirroring both the
  // `a new promise ... in the [=current Realm=]` fix (SpecPatch #4/#12) and
  // webidl/index.bs's own established idiom for the same op ("a [=new=]
  // {{DOMException}} created in the [=current realm=]", index.bs:14896).
  // The realm clause is captured as a raw `[=...=]` link (not hardcoded to
  // `current Realm`) and parsed generically via `parse`, the same way
  // `parseArgs` resolves a bare trailing `[=link=]` elsewhere in this file.
  // `{{X}}` itself is captured as a plain interface-name string and wrapped
  // in `SpecTerm` below — exactly the same node every other bare `{{X}}`
  // parses to (`BracedTerm`); what a `SpecTerm` naming a real WJI interface
  // *means* at runtime is decided in exactly one place, `Compiler.scala`'s
  // `SpecTerm`-compiling switch, not here.
  private val NewExpr =
    """(?si)^a\s+\[=/new=\]\s+\{\{([^}]+)\}\}(?:\s+object)?\s+in\s+the\s+(\[=[^\]]+=\])$""".r
  // "a {{X}} exception" / "a {{X}}" — Bikeshed's common idiom for
  // constructing a new exception object of WebIDL/spec type X (e.g. "throw
  // a {{TypeError}} exception", "reject |promise| with a {{CompileError}}
  // exception") — the trailing "exception" is frequently dropped after
  // "Throw" specifically (e.g. "Throw a {{TypeError}}.", index.bs:1477 and
  // seven more), since "Throw" alone already disambiguates "construct a new
  // one" from "an existing value of type X". Semantically the same "freshly
  // constructed instance of interface X" as NewExpr's "a [=/new=] {{X}}"
  // form above, so it reuses the same New(iface) node rather than adding a
  // new one.
  // The `{{X}}` itself is sometimes further wrapped in Bikeshed's
  // `<l spec=ecmascript>...</l>` cross-spec-link tag (e.g. "throw a <l
  // spec=ecmascript>{{TypeError}}</l>.", webidl_yet_categorized.md category
  // I-I). Checked every `<l spec=ecmascript>` site in webidl/index.bs: the
  // other ~75 occurrences sit in plain descriptive prose outside any
  // algorithm's step list, so AlgorithmExtractor never pulls them in and they
  // never reach this parser — only this one "throw a {{X}}" idiom does, so
  // the wrapper is tolerated right here rather than via a general tag-strip.
  private val NewExceptionExpr =
    """(?si)^an?\s+(?:<l spec=\w+>)?\{\{([^}]+)\}\}(?:</l>)?(?:\s+exception)?$""".r
  private val EmptyList = """(?si)^a\s+new,?\s+empty\s+(?:\[=list=\]|list)$""".r
  // "a new [=byte sequence=] of [=byte sequence/length=] equal to LENGTH" — a
  // freshly allocated all-zero byte sequence of the given length.
  private val NewByteSeqOfLength =
    """(?si)^a\s+new\s+\[=byte sequence=\]\s+of\s+\[=byte sequence/length=\]\s+equal to\s+(.+)$""".r
  // "a new {{ArrayBuffer}} with the internal slots [[X]], [[Y]], ..." — the
  // js-api Memory-buffer algorithms' bespoke ArrayBuffer construction (with a
  // custom [[ArrayBufferDetachKey]]), distinct from NewExpr's "a [=/new=]
  // {{X}}" platform-object idiom above (see docs/underspecified-behaviors.md:
  // no algorithm is actually named here for what this should call). The
  // trailing slot-name list is purely declarative — every occurrence
  // immediately `Set`s each slot right after — so it's discarded, not parsed.
  private val NewArrayBufferWithSlots =
    """(?si)^a\s+new\s+\{\{ArrayBuffer\}\}\s+with\s+the\s+internal\s+slots\s+.+$""".r
  // catch-all for "a new ..." not matched by the more specific forms above —
  // EmptyList/NewByteSeqOfLength/NewArrayBufferWithSlots (the only ones that
  // literally start with "a new") must precede this.
  private val PlainNewExpr = """(?si)^a\s+new\s+.+""".r
  private val EmptyMapProse = """(?si)^the ordered map «(?:\[\s*\])?\s*»$""".r
  // must precede ListLiteral below — «[ ... ]» would otherwise also match
  // ListLiteral's more general «...» shape, with the "[ ... ]" folded into
  // its content instead of recognized as a map literal.
  // spec error #1 (spec_errors.md): written as «  » instead of «[ ]»;
  // SpecPatch corrects it, but we match both forms for robustness
  private val MapLiteral = """(?s)^«\[\s*(.*?)\s*\]»$""".r
  private val ListLiteral = """(?s)^«\s*(.*?)\s*»$""".r
  private val TuplePat = """(?s)^\((.+)\)$""".r
  // "&lt;|operation|, |values|&gt;" — spec's angle-bracket tuple notation for
  // a multi-value algorithm result (e.g. "create an operation function",
  // webidl/index.bs:12562). The extractor works on raw .bs source (see
  // AlgorithmExtractor), so `<`/`>` reach here as literal HTML entities, not
  // decoded characters — unlike `<var ignore>`/`<sup>`/etc. above, which are
  // genuine markup tags kept verbatim in the extracted text.
  private val AngleTuplePat = """(?s)^&lt;\s*(.+?)\s*&gt;$""".r

  // "{ **min** |initial|, **max** |maximum| }" / "{ <b>[=limits|min=]</b>
  // |initial|, <b>[=limits|max=]</b> |maximum| }" — Bikeshed's "construct a
  // formal-grammar record from named fields" notation (e.g. Memory's/Table's
  // constructors building a `memtype`/`tabletype`, index.bs:876/1043). The
  // label (bold text, or a bold-wrapped `[=...=]` link) carries no
  // information `parse` needs — SpecTec's own fixed field order is what
  // determines meaning, not the prose label — so `parseBracedFields` keeps
  // only the ordered values, wrapped in the tag-less [[Expr.Seq_]] (real
  // SpecTec tag assignment is left to a later lowering pass — see that
  // node's own doc).
  private val BracedFields = """(?s)^\{\s*(.+)\s*\}$""".r
  private val BracedFieldLabel =
    """(?s)^(?:\*\*[^*]+\*\*|<b>.*?</b>)\s+(.+)$""".r

  private def parseBracedFields(inner: String): Option[Expr] =
    val values = splitComma(inner).map { seg =>
      seg.trim match
        case BracedFieldLabel(value) => Some(parse(value))
        case _                       => None
    }
    if values.forall(_.isDefined) then Some(Seq_(values.flatten)) else None

  // "[the] TNAME{\[[Field1]]: E1, \[[Field2]]: E2, ...}" — Bikeshed's
  // record/struct literal notation for a value of a named record type with
  // explicit field values (e.g. "the PropertyDescriptor{\[[Writable]]:
  // <emu-val>true</emu-val>, \[[Value]]: |constructor|}",
  // webidl_yet_categorized.md category I-C). Unlike BracedFields just above
  // (an unnamed, purely positional `{ ... }`), each field here is named (the
  // `\[[...]]` slot it initializes) and the whole literal carries its own
  // type name immediately before `{`, so it parses to [[Expr.RecordLit]]
  // rather than [[Expr.Seq_]].
  private val RecordLitPrefix =
    """(?si)^(?:the\s+)?([A-Za-z][A-Za-z0-9]*)\{(.+)\}$""".r
  private val RecordFieldPat =
    """(?s)^\\?\[\[([^\]]+)\]\]\s*:\s*(.+)$""".r

  private def parseRecordFields(inner: String): Option[List[(String, Expr)]] =
    val fields = splitComma(inner).map { seg =>
      seg.trim match
        case RecordFieldPat(name, valueRaw) => Some(name -> parse(valueRaw))
        case _                              => None
    }
    if fields.forall(_.isDefined) then Some(fields.flatten) else None

  // ---- Call syntax: explicit invocation of a named algorithm/AO ----

  private val JSCallFull = """(?s)^\[\$([^\$]+)\$\]\((.*)\)$""".r
  // "<a abstract-op>Name</a>(args...)" — Bikeshed's *other* spelling for an
  // explicit abstract-op call, used alongside (not instead of) `[$Name$](...)`
  // — webidl/index.bs mixes both freely throughout, `<a abstract-op>` ~120
  // times against `[$...$]`'s ~183, so this isn't a deviant one-off, it's a
  // second established convention this parser needs to know natively (see
  // webidl_yet_categorized.md category I-I). Same unambiguous "(...)" call
  // shape as JSCallFull, so it parses identically straight to JSCall — no new
  // Expr node needed, and AssociatedWithAttrPat's existing JSCall/AlgoCall
  // match arm picks it up for free. Nested calls (e.g. "<a
  // abstract-op>Completion</a>(<a abstract-op>Call</a>(...))",
  // webidl/index.bs:14653) compose automatically: splitComma/TextSplit
  // already track paren depth, so the captured arg string round-trips back
  // through `parse` and hits this same case again.
  private val AbstractOpCallFull =
    """(?s)^<a abstract-op>([^<]+)</a>\((.*)\)$""".r
  // unlike LinkProse/LinkOnly below, the explicit `(...)` here is
  // unambiguous call syntax (mirrors JSCallFull) — no term/value
  // reference is ever written this way — so this can go straight to
  // AlgoCall without waiting for ResolveLinksPass.
  private val LinkFull = """(?s)^(\[=(?:(?!=\]).)+?=\])\s*\((.*)\)$""".r
  // "passing ARG1[, ARG2, ...] to the [=LINK=]" — WebIDL's own idiom for
  // invoking an algorithm with an unnamed positional argument list, the
  // link named *last* rather than first (contrast LinkProse/LinkFull, and
  // StepsCallWithArgs's named "with X as this and Y as the argument
  // values" idiom) — e.g. "the result of passing |S| and |args| to the
  // [=overload resolution algorithm=]" (the leading "the result of " is
  // already stripped by ResultOf by the time this is tried;
  // webidl_yet_categorized.md category III-B). Same unambiguous shape as
  // LinkFull, so this goes straight to AlgoCall without waiting for
  // ResolveLinksPass. ARGS is loose prose, not a clean comma list (just
  // "|S| and |args|" here), so it's split with parseArgs — the same
  // shrink-based extractor LinkProse itself uses for its own trailing
  // prose — rather than a bespoke comma/"and" splitter.
  private val PassingToCall =
    """(?si)^passing\s+(.+?)\s+to\s+the\s+(\[=(?:(?!=\]).)+?=\])$""".r
  // "[=LINK=], passing ARG1, ARG2, and ARG3" — the same positional idiom as
  // PassingToCall with the link named *first* (e.g. "[=perform a security
  // check=], passing |jsValue|, |attribute|'s [=identifier=], and "getter"").
  // Must precede LinkProse, whose parseArgs would otherwise let a greedy
  // suffix-anchored pattern (PossessiveIdentifier's "X's [=identifier=]")
  // swallow ", passing |jsValue|, |attribute|" as its own base. ARGS here is
  // a clean "A, B, and C" list, so it's split on top-level commas instead.
  private val LinkPassingCall =
    """(?si)^(\[=(?:(?!=\]).)+?=\]),?\s+passing\s+(.+)$""".r
  private val LeadingAnd = """(?si)^and\s+""".r
  private def parsePassingArgs(argsRaw: String): List[Expr] =
    splitComma(argsRaw).map(a => parse(LeadingAnd.replaceFirstIn(a, "")))

  /** [[LinkPassingCall]] as a (link, args) pair — shared with `InstrParser`'s
    * `Perform` call parsing, which splits a leading `[=link=]` from its
    * arguments itself rather than going through [[parse]].
    */
  private[wji] def parseLinkPassingCall(
    raw: String,
  ): Option[(String, List[Expr])] = raw.trim match
    case LinkPassingCall(link, argsRaw) =>
      Some((normalizeLink(link), parsePassingArgs(argsRaw)))
    case _ => None
  // "the result of creating a/an [=LINK=] given ARG1[, ARG2, ...]" — WebIDL's
  // idiom for invoking an algorithm whose own dfn is the *thing it creates*
  // ("The <dfn>attribute getter</dfn> is created as follows, given ..."),
  // not a verb phrase — e.g. "the result of creating an [=attribute getter=]
  // given |attr|, |definition|, and |realm|" (webidl_yet_categorized.md
  // category III-B). Must precede ResultOf, which would otherwise strip
  // "the result of creating " and leave a bare "an [=LINK=] given ..." noun
  // phrase indistinguishable from a term reference. Same unambiguous shape
  // as PassingToCall, and same parseArgs split for its loose "A, B, and C"
  // prose.
  private val CreatingLinkGivenCall =
    """(?si)^the result of creating\s+an?\s+(\[=(?:(?!=\]).)+?=\])\s+given\s+(.+)$""".r
  // "the [=interface object=] of |I| in |realm|" / "the [=interface
  // prototype object=] for |I| in |realm|" — a WebIDL glossary term (the
  // cached per-realm *value*, not the algorithm that builds it) referenced
  // with its own subject/realm pair, unlike LinkOnly's bare "the [=LINK=]"
  // or LinkProse's no-leading-"the" "[=LINK=] REST" (webidl_yet_categorized.md
  // category III-A). `link` here never names a real algorithm directly —
  // `ResolveLinksPass`'s `cachedObjects` resolves it to a lookup into the
  // per-realm cache of interface (prototype) objects. Args are kept to plain
  // `|var|`s (not general `.+`), matching every occurrence seen so far and
  // this file's convention elsewhere (`AssociatedRealm`, `PossessiveIdentifier`,
  // `IdentifierOfType`) of not over-generalizing past the observed shape.
  private val LinkOfForIn =
    """(?si)^the\s+(\[=[^\]]+=\])\s+(?:of|for)\s+(\|[^|]+\|)\s+in\s+(\|[^|]+\|)$""".r
  // "the [=interface prototype object=] of that [=inherited interface=] in
  // |realm|" — webidl/index.bs:12055-12056, after `SpecPatch` reorders the
  // original "... in |realm| of that [=inherited interface=]" (word order
  // deviates from every other reference to this same term in this document,
  // e.g. index.bs:12030's "of [=interface=] |I| in |realm|" — see
  // docs/spec_inconsistencies.md) into `LinkOfForIn`'s own canonical "of X in
  // realm" order. Unlike `LinkOfForIn`'s own subject (always a bare `|var|`),
  // this one is the anaphoric noun phrase "that [=inherited interface=]" —
  // refers back to whichever interface the enclosing `#2-2` condition
  // ("|interface| is declared to inherit from another interface",
  // `CondParser.DeclaredToInheritPos`) already established, which in the one
  // real corpus occurrence of this exact phrasing is always
  // `create_an_interface_prototype_object`'s own `interface` parameter (the
  // same anaphor `InterfaceInheritsFrom`'s own `.inherit` field mapping below
  // resolves for a named `|var|`) — hardcoded here, matching this file's
  // convention for narrow, single-occurrence idioms (`ValidTypeLink`,
  // `ExistsSuchThat`) rather than threading condition-parsed context into
  // this call. Produces a plain `Link`, exactly `LinkOfForIn`'s own output
  // shape, so `ResolveLinksPass`'s existing `cachedObjects` resolves it
  // identically to that occurrence — no `ResolveLinksPass` change needed.
  private val LinkOfInheritedInterfaceIn =
    """(?si)^the\s+(\[=[^\]]+=\])\s+of that \[=inherited interface=\]\s+in\s+(\|[^|]+\|)$""".r
  // "the [=interface object=] of |P| with identifier |P|'s [=identifier=] in
  // |realm|" — after `SpecPatch` spells out `create an interface object`'s
  // (webidl/index.bs:11933-11936) required `id` parameter, elided by every
  // real call site (docs/spec_errors.md). Produces a 3-arg `Link` — unlike
  // `LinkOfForIn`'s 2 — `ResolveLinksPass.cachedObjects` takes the explicit
  // `id` as the cache key. The backreference `\2` requires both mentions of the subject to
  // name the same variable, matching exactly what the patched text always
  // says.
  private val LinkOfWithIdentifierIn =
    """(?si)^the\s+(\[=[^\]]+=\])\s+of\s+(\|[^|]+\|)\s+with identifier \2's \[=identifier=\]\s+in\s+(\|[^|]+\|)$""".r
  private val LinkProse = """(?s)^(\[=(?:(?!=\]).)+?=\])\s+(.+)$""".r
  private val LinkOnly = """(?s)^(?:the\s+)?(\[=(?:(?!=\]).)+?=\])$""".r
  // "VALUE, [=link=]" — spec's passive-voice idiom for a unary conversion
  // applied to the value stated just before it (e.g. "|result|,
  // [=converted to a JavaScript value=]", "|map|'s [=map/size=], [=converted
  // to a JavaScript value=]." — both from webidl/index.bs). Mirrors
  // DotFieldLink's `base.[=name=]` shape but with a comma rather than a dot;
  // unlike DotFieldLink (a field read), this is a call, so it produces a
  // Link (resolved to AlgoCall/Case by ResolveLinksPass), not Field. Its
  // trailing separator (",") is disjoint from every other call/field-access
  // pattern's own trailing separator, so it can go anywhere in this list.
  private val TrailingLinkCall = """(?s)^(.+),\s*(\[=(?:(?!=\]).)+?=\])$""".r

  // ---- Bare references ----

  private val ThisOnly = """(?s)^\*\*this\*\*$""".r
  // "the <emu-val>this</emu-val> value" — the JS-level receiver, as WebIDL's
  // own binding algorithms spell it (webidl/index.bs:12355, 12406, 12545) —
  // must precede the generic EmuVal below.
  private val ThisValue = """(?si)^the\s+<emu-val>this</emu-val>\s+value$""".r
  // "the passed arguments" — see Expr.ArgumentsList.
  private val PassedArguments = """(?si)^the\s+passed\s+arguments$""".r
  // "{{NewTarget}}" — see Expr.NewTarget — must precede the generic
  // BracedTerm below, which would otherwise turn it into a SpecTerm.
  private val NewTargetOnly = """(?s)^\{\{NewTarget\}\}$""".r
  // "it" — see Expr.Pronoun.
  private val PronounOnly = """(?i)^it$""".r
  // WebIDL's implicit setter argument (see Expr.GivenValue) — must precede
  // the generic BoldConst below, which would otherwise swallow it into a
  // meaningless SpecTerm.
  private val GivenValueOnly = """(?s)^\*\*the given value\*\*$""".r
  private val VarOnly = """(?s)^\|([^|]+)\|$""".r
  private val VarIgnore = """(?s)^<var\s+ignore>([^<]*)</var>$""".r

  // ---- Arithmetic & casts ----

  // binary operators, matched at the top level only (see BinOpSeps' use with
  // findLastTopLevelAny below) — tried before SlotAccess/DotFieldLink/
  // PossessiveSlot/IndexBy* so their own greedy "everything to the left is
  // the base" patterns can't swallow a top-level operator into their base
  // (e.g. "|newLength| - |buffer|.\[[ArrayBufferByteLength]]" must split as
  // "|newLength| - (|buffer|.[[ArrayBufferByteLength]])", not
  // "(|newLength| - |buffer|).[[ArrayBufferByteLength]]" — field/slot access
  // binds tighter than arithmetic). The *last* top-level occurrence is used,
  // not the first, so a chain (e.g. "a + b - c") still recurses out
  // left-associatively.
  private val BinOpSeps =
    Seq(" modulo ", " + ", " - ", " * ", " &div; ", " &minus; ")
  private def parseBOp(op: String): BOp = op.trim match
    case "+"             => BOp.Add
    case "-" | "&minus;" => BOp.Sub
    case "*"             => BOp.Mul
    case "&div;"         => BOp.Div
    case "modulo"        => BOp.Mod
  private val AsMathPat =
    """(?si)^(.+)\s+interpreted as a \[=mathematical value=\]$""".r
  private val AsWasmPat =
    """(?si)^(.+)\s+as a WebAssembly \[=(\w+)=\]$""".r
  // "|number| rounded to the nearest representable value using IEEE
  // 754-2019 round to nearest, ties to even mode" -- `ToWebAssemblyValue`'s
  // f32 case (index.bs:1438), the corpus's one occurrence. Parses to plain
  // `inner` unchanged (the qualifier is dropped, not wrapped in a node of its
  // own): the rounding only becomes *observable* once the bound name is used
  // to build a concrete `f32.const` wasm value (the very next step here,
  // "Return [=f32.const=] |f32|."), so it's `state.util.wasmF32Const`'s own
  // `d.toFloat` narrowing conversion (JLS 5.1.3, itself already IEEE-754
  // round-to-nearest-ties-to-even) that does the one rounding that actually
  // matters, right there at that single point -- see its own doc,
  // `docs/hardcodes.md` #19. Binding `f32` to the still-unrounded value here
  // is observably identical, since nothing else ever reads `f32` in between.
  private val RoundedToNearestRepresentable =
    """(?si)^(.+)\s+rounded to the nearest representable value using IEEE 754-2019 round to nearest, ties to even mode$""".r
  private val PowPat = """(?s)^(\d+)<sup>(.+?)</sup>$""".r
  private val NegPat = """(?s)^[-−](.+)$""".r

  // ---- Structural access: field/slot/index/length/association off a base
  // value ----

  private val SlotAccess = """(?s)^(.+)\.\\?\[\[([^\]]+)\]\]$""".r
  // "BASE.[[Slot]](ARGS)" — an internal slot/method invoked immediately, e.g.
  // "|unforgeables|.[[GetOwnProperty]](|key|)" (webidl/index.bs:13853,
  // webidl_yet_categorized.md category I-A). Not just a SlotAccess (which
  // anchors at the closing `]]`) with leftover trailing text — and not the
  // `ClosureCall(Field(base, name), args)` the StepsCallWithArgs/
  // StepsCallNoArgs idiom below produces either: this syntax is an internal
  // *method* call, which passes BASE itself as the receiver, so it gets its
  // own `MethodCall` node (see its doc).
  private val SlotMethodCall =
    """(?s)^(.+)\.\\?\[\[([^\]]+)\]\]\((.*)\)$""".r
  // a bare "\[[SlotName]]" with no base — the slot's *name*, used as a value
  // rather than read off a specific object (e.g. CreateBuiltinFunction's
  // `additionalInternalSlotsList` argument, « \[[FunctionAddress]] »).
  // Mirrors mainline `esmeta.compiler.Compiler`'s own `InternalSlots` XRef
  // handling, which likewise compiles a bare slot name to `EStr`.
  private val BareSlotName = """(?s)^\\?\[\[([^\]]+)\]\]$""".r
  // a raw Bikeshed section-anchor reference, e.g. "[[#platform-object-setprototypeof]]"
  // (webidl/index.bs:13861, webidl_yet_categorized.md category I-B) — kept
  // as a neutral `Link` for ResolveLinksPass to map to the `<div algorithm>`
  // that section defines. Must precede BareSlotName, whose `[[...]]` shape it
  // would otherwise match as a slot name.
  private val AnchorLink = """(?s)^(\[\[#[\w-]+\]\])$""".r
  private val PossessiveSlot =
    """(?si)^the value of (.+)'s \\?\[\[([^\]]+)\]\] internal slot$""".r
  // "the value of the [[Slot]] slot of BASE" — same PossessiveSlot concept,
  // slot-name-first word order (webidl/index.bs:13849-13850,
  // webidl_yet_categorized.md category I-A), kept beside PossessiveSlot the
  // same way LengthOf/ElementCount/PossessiveSize below group sibling
  // phrasings of one concept together.
  private val TheSlotOf =
    """(?si)^the value of the \\?\[\[([^\]]+)\]\] slot of (.+)$""".r
  // e.g. "|module|.[=imports=]" — a WebAssembly-spec record field written
  // with a dot, where the `[=...=]` is a documentation link on the field
  // name rather than a call (contrast with `LinkFull`/`LinkProse`,
  // which require the string to *start* with `[=`).
  private val DotFieldLink = """(?s)^(.+)\.(\[=(?:(?!=\]).)+?=\])$""".r
  // "BASE.field" — a plain, undecorated record-field access, the Wasm Core
  // Spec's own formal notation (e.g. `store.funcs`, mirroring `S.FUNCS`)
  // written directly in prose rather than through a Bikeshed dfn-link
  // (contrast SlotAccess's `.[[slot]]`/DotFieldLink's `.[=field=]` above,
  // both tried first so a decorated dot-suffix isn't mistaken for this).
  private val DotField = """(?s)^(.+)\.([A-Za-z][A-Za-z0-9]*)$""".r
  // three phrasings that all normalize to the same Length node — kept as
  // separate patterns since spec prose spells "length" three different ways,
  // but grouped here so a fourth phrasing gets added next to its siblings
  // rather than off on its own.
  private val LengthOf =
    """(?si)^the (?:\[=(?:string/length|list/size)=\]|length) of (.+)$""".r
  private val ElementCount = """(?si)^the number of elements in (.+)$""".r
  // "X prefixed with LITERAL" (index.bs:488/1849, both "|builtinSetName|
  // prefixed with "wasm:"") — string concatenation, `LITERAL` first.
  private val PrefixedWith = """(?si)^(.+?)\s+prefixed with\s+(.+)$""".r
  // "the concatenation of X and Y" (index.bs:1994, js-string's
  // fromCharCodeArray) — string concatenation, same Expr.Concat node
  // PrefixedWith already builds, just a different phrasing/arg order.
  private val ConcatenationOf =
    """(?si)^the concatenation of (.+?) and (.+)$""".r
  // must precede PossessiveAssociation below — "the X's [=list/size=]"
  // would otherwise also match its more general "'s [=link=]" shape.
  private val PossessiveSize = """(?si)^(.+)'s \[=list/size=\]$""".r
  private val ElementAt =
    """(?si)^the value of the element stored at index (.+) in (.+)$""".r
  // js-string's intoCharCodeArray writes the same "index X in Y" shape as a
  // `Set` target ("Set the element at index |start| + |i| in |array| to
  // ...") without ElementAt's "the value of ... stored" framing -- same
  // Index(arr, idx) node either way (`Instr.Set`'s LHS is parsed by this same
  // `ExprParser.parse`, see `InstrParser.SetPrefix`), just needs its own
  // pattern so it's tried before the generic BinOp fallback would otherwise
  // wrongly split "|start| + |i|" out of the whole reference.
  private val ElementAtRef =
    """(?si)^the element at index (.+) in (.+)$""".r
  // "the index of LIST where ELEM is found" (index.bs:1255) — see
  // Expr.IndexOf / ExpandIndexOfPass.
  private val IndexOfPat =
    """(?si)^the index of (.+) where (.+) is found$""".r
  // "the shortest argument list ... of/in the entries in/of BASE" — the
  // argument/type-list projection of whichever effective-overload-set entry
  // in BASE has the fewest elements (webidl/index.bs:12584; "type list"
  // variant at webidl/index.bs:11529, unextracted but tolerated for free).
  // See Expr.ShortestArgumentList / ExpandShortestArgumentListPass.
  private val ShortestArgumentListOfEntries =
    """(?si)^the shortest (?:argument|type) list (?:of|in) the entries (?:in|of) (.+)$""".r
  // "the [=list=] of [=regular operations=] that are [=members=] of |X|" —
  // webidl/index.bs's `define the regular operations`/`define the
  // operations` member-list projection. `esmeta.wji.Initialize` builds each
  // WJI `Definition`'s operations directly as an `.operations` field on the
  // record it hands to `create_a_namespace_object`, so this reduces to a
  // plain field read — no separate interface/attribute/member record model
  // needed for WJI's namespace-only reachable scope. Lowercase, matching the
  // interface/namespace-member field naming convention (see
  // [[fieldFromLink]]).
  // optionally qualified by "[=unforgeable=]" (webidl/index.bs:12316, 12514,
  // `define the unforgeable regular attributes/operations`).
  private val MemberOfDefinition =
    """(?si)^the \[=list=\] of (\[=unforgeable=\] )?\[=([^\[]+)=\] that are \[=members=\] of (.+)$""".r
  // "|op|'s [=identifier=]" — webidl/index.bs's dfn for an operation's/
  // attribute's name; `esmeta.wji.Initialize.seedHostDefined`'s
  // `operationRecord`/`attributeRecord` both store this under the literal
  // field name `id` (matching `esmeta.wji.lang`'s own `WjiOperation`/
  // `WjiAttribute.id`), not `identifier` — so, like AssociatedRealm below,
  // this needs its own mapping rather than falling through to the generic
  // `fieldFromLink` (which would read the dfn text as-is). Must precede
  // PossessiveAssociation below for the same reason AssociatedRealm does.
  private val PossessiveIdentifier = """(?si)^(.+)'s \[=identifier=\]$""".r
  // "|attribute|'s type" — the IDL type an attribute was declared with
  // (webidl/index.bs:12339, 12368, 12379, 12399, 12434;
  // webidl_yet_categorized.md category II-G). Plain prose, not a dfn link, so
  // nothing else here would pick it up; `Initialize.seedHostDefined`'s
  // `attributeRecord` stores it under `ty` (matching `WjiAttribute.ty`).
  // Restricted to a bare variable base: "... to an IDL value of |attribute|'s
  // type" (index.bs:12459) ends the same way but isn't this field read.
  private val PossessiveType = """(?si)^(\|[^|]+\|)'s type$""".r
  // "the string "<code>get </code>" prepended to |attribute|'s
  // [=identifier=]" — the name `attribute getter`/`attribute setter`
  // (webidl/index.bs:12382, 12466) give the function they create
  // (webidl_yet_categorized.md category I-J). The quoted prefix is a string
  // literal like any other (`QuotedCodeStr`/`QuotedStr`); the rest is whatever
  // string-valued expression it's prepended to. Must precede
  // PossessiveIdentifier above, whose greedy base would otherwise swallow the
  // whole "the string ... prepended to |attribute|" as the thing whose
  // identifier is read.
  private val StringPrependedTo =
    """(?si)^(?:the string\s+)?("[^"]*")\s+prepended to\s+(.+)$""".r
  // "the identifier of interface |I|" — "create an interface object"'s own
  // phrasing of the same `id` field PossessiveIdentifier above maps ("X's
  // [=identifier=]") — webidl_yet_categorized.md category II-E's `#3-4`.
  // "the identifier of ..." itself shows up elsewhere in webidl/index.bs too,
  // but only ever in prose/notes, not algorithm step text this parser ever
  // sees — "interface" is the only type-tag word actually observed here
  // (webidl_yet_summary.md), but kept general (any bare word, not hardcoded
  // to "interface") the same way MemberOfDefinition/PossessiveIdentifier
  // don't hardcode a specific kind either — the tag is discarded regardless
  // of what it says, same as PossessiveIdentifier ignores its own base's
  // type.
  private val IdentifierOfType =
    """(?si)^the identifier of \w+ (\|[^|]+\|)$""".r
  // "the [=interface=] that |I| [=interface/inherits=] from, if any, and null
  // otherwise" — webidl/index.bs's "inclusive inherited interfaces"
  // (line 715) reads the interface |I| was declared to inherit from (e.g.
  // `interface Foo : Bar { ... }`). `[=interface/inherits=]` is not itself a
  // callable algorithm (webidl/index.bs:634 — it's descriptive prose about
  // IDL declaration syntax, never a `<div algorithm>`), so this reads
  // straight through to `Definition.inherit` (`None` for every interface in
  // the current corpus, since none declares one) via the same `inherit`
  // field `Initialize.seedHostDefined` mirrors onto each `HOST_DEFINED`
  // record, rather than treating it as an AO call with nothing behind it.
  private val InterfaceInheritsFrom =
    """(?si)^the \[=interface=\] that (\|[^|]+\|) \[=interface/inherits=\] from, if any, and null otherwise$""".r
  // "the IDL [=interface type=] value that represents a reference to
  // |jsValue|" (webidl_yet_categorized.md category I-N's `#6-11`, `#7-21`,
  // `#10-9`) — an IDL interface type value *is* a reference to the platform
  // object (webidl/index.bs:7961-7962 converts it back to "the same object
  // that the IDL interface type value represents"), and this project has no
  // separate IDL-value representation for it, so it reads straight through
  // to the referenced value itself.
  private val InterfaceTypeValueOf =
    """(?si)^the IDL \[=interface type=\] value that represents a reference\s+to (\|[^|]+\|)$""".r
  // "|realm|'s [=is global prototype chain mutable=]" (webidl_yet_categorized.md
  // category II-J) — a real Realm field (webidl/index.bs:10226-10229), seeded
  // once by `esmeta.wji.Initialize` onto the sole Realm Record at
  // `esmeta.es.builtin.realmAddr` (see that seed's own doc for why exactly one
  // instance of this field ever needs seeding, and why it's always `false`).
  // Reads exactly like `AssociatedRealm` below — `Field(parse(baseRaw), name)`
  // — rather than compiling to a literal, so a future `ShadowRealm`
  // implementation (the only spec mechanism that ever sets this field `true`)
  // would only need to change what `Initialize` seeds, not this parser rule.
  // `baseRaw` is discarded structurally (parsed like any other receiver, not
  // specially inspected) since every realm reference in this single-realm
  // pipeline resolves to the same `realmAddr` regardless of spelling — this
  // also transparently covers the chained `|O|'s [=associated realm=]'s
  // [=is global prototype chain mutable=]` form at webidl/index.bs:12242/13970
  // (not yet extracted by this project): `AssociatedRealm`'s own
  // `Field(_, "Realm")` read lands on `realmAddr` too, so chaining this rule's
  // `Field` on top of it still reads the one seeded field correctly. Must
  // precede AssociatedRealm below: its stricter suffix match (ending
  // `[=is global prototype chain mutable=]`, not `[=associated Realm=]`) still
  // overlaps the same "X's [=Y=]" shape space, and Scala's `match` tries cases
  // in source order.
  private val IsGlobalPrototypeChainMutable =
    """(?si)^(.+)'s \[=is global prototype chain mutable=\]$""".r
  // "|realm|'s [=realm/global object=]" — a Realm Record's real ECMA-262
  // [[GlobalObject]] field, read straight through like AssociatedRealm below.
  private val RealmGlobalObject =
    """(?si)^(.+)'s\s+\[=realm/global object=\]$""".r
  private val AssociatedRealm = """(?si)^(.+)'s \[=associated Realm=\]$""".r
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
  // is ever passed here as `func`. Must precede PossessiveAssociation below
  // — "the X's [=associated Realm=]" would otherwise also match its more
  // general "'s [=link=]" shape.
  private val PossessiveAssociation =
    """(?si)^the (.+)'s (?:associated )?(\[=(?:(?!=\]).)+?=\])$""".r
  // "[=comp-type/func=] |parameters| → |results|" — SpecTec's comptype arrow
  // notation for a functype (`al_of_comptype`'s `FuncT (rt1, rt2) -> CaseV
  // ("->", [rt1; rt2])`), corrected to include the `FUNC` discriminator
  // `.spectec`'s current `comptype` grammar requires (`comptype ::= STRUCT
  // ... | ARRAY ... | FUNC resulttype -> resulttype`,
  // `1.2-syntax.types.spectec:117`) — see `docs/spec_errors.md` #18. Every
  // real occurrence in js-api/index.bs predates that grammar (from before
  // `comptype` existed at all, when a functype's own arrow notation needed no
  // discriminator to tell it apart from a struct/array shape) and is
  // SpecPatch-corrected to this form. The tag is kept as the raw link text
  // here (`"[=comp-type/func=]"`), not resolved to `"FUNC"`/`"->"` — that's
  // `NormalizeSpecTecCaseShapePass.RenamedTag`'s job, same layering as every
  // other SpecTec-runtime-tag lookup in this file. Its match arm lives in the
  // "Construction" section below — `parse` handles each side generically
  // there (a bare `|var|`, a `«...»` list literal, or `<var ignore>X</var>`),
  // covering both this pattern's use as a `Let` LHS
  // (`ExpandDestructuringLetPass`, which alone treats a literal empty `« »`
  // side as "nothing to bind" rather than requiring every side to be a
  // `Var`) and as an ordinary expression building a fresh functype value
  // (`tag_alloc`'s argument, via `fold`).
  private val CompTypeArrowPrefix = "[=comp-type/func=] "
  // "`func |builtinFuncType|`" (index.bs:1905, the only occurrence — its
  // sibling backtick externtype literal, "`global const (ref extern)`" at
  // index.bs:408, needed real multi-token restructuring via `SpecPatch`
  // instead, see `docs/spec_inconsistencies.md` #21) — SpecTec's externtype
  // discriminator wrapping a deftype (`externtype ::= FUNC deftype | TABLE
  // ... | MEM ... | GLOBAL ... | TAG ...`), same "FUNC"/"GLOBAL"/... tag
  // convention `stringExternType`'s already-working `Case("GLOBAL", [Case(
  // "", [mut, reftype])])` construction uses. Matched against the already
  // backtick-stripped inner text (`Backticked`'s own `parse(inner)`
  // recursion), not the raw backtick-wrapped form.
  private val FuncExternType = """(?si)^func\s+(.+)$""".r
  private val IndexByStr = """(?s)^(.+)\["([^"]+)"\]$""".r
  private val IndexByVar = """(?s)^(.+)\[(\|[^|]+\|)\]$""".r
  private val IndexByNum = """(?s)^(.+)\[(-?\d+)\]$""".r
  // a general `base[EXPR]` index whose key is a compound expression rather than
  // a bare string/var/number (e.g. `|bytes|[|i| &minus; |offset|]`). The
  // `&minus;`/`+` inside the brackets is at bracket-depth 1, so the BinOp
  // case above (tried first, but findLastTopLevelAny-based and thus
  // bracket-aware) correctly steps aside and leaves the whole bracketed
  // suffix for this pattern instead. The base must end in a non-space char (index
  // syntax is written `base[key]` with no gap, so a space-then-`[` like the
  // `→ [...]` in a func-type destructuring is not an index) and the key must
  // not start with `=` (so a trailing `[=link=]` documentation link such as
  // `|func|'s [=associated Realm=]` is not mistaken for an index). Placed after
  // the specific IndexBy* forms above (though, since `parse` re-derives the
  // same Str/Var/Num from the raw bracket content either way, the four
  // IndexBy* forms actually produce identical results regardless of their
  // relative order).
  private val IndexByExpr = """(?s)^(.+\S)\[([^\[\]=][^\[\]]*)\]$""".r

  // ---- Noun-phrase descriptions: not-yet-evaluable, or a narrow single-arg
  // call ----

  // "a [=Data Block=] which is [=identified with=] the underlying memory of
  // |memaddr|" — the one "which ..." phrasing that needs its own dedicated
  // node (Expr.DataBlockOf) rather than falling into the generic
  // RelativeClauseDesc/Described below, which also covers unrelated "which
  // ..." shapes (e.g. "a [=host function=] which executes |steps| when
  // called"). Tried first so RelativeClauseDesc doesn't swallow it.
  private val DataBlockIdentifiedWith =
    """(?si)^a\s+\[=Data Block=\]\s+which\s+is\s+\[=identified with=\]\s+the\s+underlying\s+memory\s+of\s+(\|[^|]+\|)$""".r
  // "a [=X=] which ..." — a relative-clause description of X, not a call
  // (e.g. "a [=host function=] which executes |steps| when called"). Not yet
  // evaluable (see Expr.Described); matched explicitly (rather than left to
  // fall through to the default `Unknown` case) so it can never be mistaken
  // for LinkIndefVar below. DataBlockIdentifiedWith above must be tried
  // first — it's the one "which ..." phrasing that needs its own node.
  private val RelativeClauseDesc =
    """(?si)^an?\s+(\[=(?:(?!=\]).)+?=\])\s+which\s+(.+)$""".r
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
  // "for constructors" — `compute the effective overload set`'s kind
  // argument at `create an interface object`'s two call sites
  // (webidl/index.bs:11948,11975: "[=Compute the effective overload set=]
  // for constructors with [=identifier=] |id| on ..."). Unlike the
  // `[=regular operations=]`/`[=static operations=]` kinds (already links,
  // so they parse to a `SpecTerm` on their own), this one is plain unlinked
  // prose and would otherwise be dropped by `parseArgs` as unparseable
  // words, so it's mapped to the `Constructor` enum explicitly.
  private val ForConstructors = """(?i)^for\s+constructors$""".r
  // "(a|an|the) <desc> such that <cond>" — any definite/indefinite/superlative
  // description satisfying a predicate, not a call. Covers all the variants
  // seen in the spec: "a [=host address=] |hostaddr| exists such that ...",
  // "the unsigned integer such that |i64| is [=signed_64=](|u64|)", "an
  // implementation-defined integer such that ...", "the smallest address
  // such that ...". `desc` (non-greedy up to the first "such that") may or
  // may not itself contain a `[=link=]`/`|var|`/qualifier word like "exists"
  // or "smallest" — kept as raw text since the phrasing varies too much to
  // structure further; `cond` (everything after), by contrast, is parsed via
  // `CondParser.parse` right here (one of two places `ExprParser` calls into
  // `CondParser`, alongside `Conditional` below — a deliberate mutual
  // reference, mirroring `CondParser`'s own existing calls back into
  // `ExprParser` for every condition's sub-`Expr`s, and `CondPrinter`/
  // `ExprPrinter`'s identical mutual shape). The whole `SuchThat` node is
  // still not directly evaluable (see `Expr.SuchThat`); matched explicitly
  // for the same reason as RelativeClauseDesc above.
  private val SuchThatDesc =
    """(?si)^(?:the|an?)\s+(.+?)\s+such\s+that\s+(.+)$""".r

  // "EXPR if COND[,] (and|or) EXPR otherwise" — WebIDL's conditional
  // expression idiom (webidl_yet_categorized.md category I-G), e.g.
  // "<emu-val>false</emu-val> if |op| is [=unforgeable=] and
  // <emu-val>true</emu-val> otherwise" (the value of a `Let`, once
  // `InstrParser` has already split off "Let |modifiable| be "). `cond` may
  // itself contain its own top-level "and"/"or" (e.g. "it is not
  // <emu-val>null</emu-val> or <emu-val>undefined</emu-val>"), so the second
  // `EXPR` is split off at the *last* top-level "and"/"or" before the
  // trailing "otherwise", not the first — `cond`'s own connectives always
  // sit earlier in the text than the one actually separating it from the
  // second `EXPR`. Deliberately requires the text to end in a bare
  // "otherwise" with nothing after it, so "..., or the following steps
  // otherwise:" (introducing a closure with its own nested sub-steps) is
  // left unmatched and falls through to whatever later case (if any)
  // recognizes its own pieces.
  private val TrailingOtherwise = """(?i)\s+otherwise:?$""".r
  private val CondValueSeps = Seq(" and ", " or ")

  private def conditionalParts(s: String): Option[(String, String, String)] =
    findTopLevel(s, " if ").flatMap { ifIdx =>
      val thenRaw = s.substring(0, ifIdx)
      val afterIf = s.substring(ifIdx + 4)
      TrailingOtherwise.findFirstMatchIn(afterIf).flatMap { m =>
        val beforeOtherwise = afterIf.substring(0, m.start)
        findLastTopLevelAny(beforeOtherwise, CondValueSeps).map {
          case (sepIdx, sep) =>
            (
              thenRaw.trim.stripSuffix(",").trim,
              beforeOtherwise.substring(0, sepIdx).trim.stripSuffix(",").trim,
              beforeOtherwise.substring(sepIdx + sep.length).trim,
            )
        }
      }
    }

  // "EXPR1 (if COND1) or EXPR2 (if COND2) [or ...]" — the *other* half of
  // `Expr.Conditional` (see its own doc): every branch carries its own
  // explicit "(if COND)" guard and there's no trailing "otherwise" at all
  // (webidl/index.bs:12558-12559). `orConditionalBranch` reads one "EXPR (if
  // COND)" segment, using `findTopLevel` (not a greedy `(...)`-matching
  // regex) to locate the "(if " that opens the guard and the "if"-body's own
  // matching close-paren — greedy paren-matching is exactly what mis-parsed
  // this idiom in the first place (see git history: it let a single `(...)`
  // pattern span clean across *two* separate guards, from the first "(" to
  // the *last* ")" in the sentence, swallowing the "or"-connector and second
  // branch into unparsed garbage). An optional leading "for " is stripped
  // per branch (as in the real occurrence's "for [=regular operations=]
  // ...", "for [=static operations=] ...") since it's pure connective
  // prose, not part of the guarded value.
  //
  // `orConditionalChain` repeats `orConditionalBranch` across every
  // top-level " or "-joined segment, requiring *at least two* — a single
  // "(if COND)" isn't this idiom (and must fall through to whatever else
  // would otherwise parse it, e.g. a plain parenthesised clause), only a
  // repeated *chain* of them is. Every real occurrence has exactly two
  // branches, but nothing here hardcodes that.
  //
  // Unlike every other pattern in this file, this one is invoked from
  // `parseArgs` rather than tried as an ordinary `parse` case:
  // `parseArgs`'s word-by-word tokenizer always tries the *longest*
  // remaining suffix first, and `LinkProse` (an unconditional "[=link=]
  // <anything>" match) would already have swallowed the entire rest of the
  // sentence — including whatever the *outer* call's later arguments are —
  // before a `parse`-level case for this shape ever got a chance to be
  // tried on just its own, shorter span. So `parseArgs` calls this directly,
  // as a bounded prefix match tried before its own generic per-token loop,
  // letting it consume exactly this shape and then keep tokenizing whatever
  // follows.
  private def orConditionalBranch(text: String): Option[(Cond, Expr, Int)] =
    val stripped = if text.startsWith("for ") then text.drop(4) else text
    val skipped = text.length - stripped.length
    findTopLevel(stripped, "(if ").flatMap { openIdx =>
      val exprRaw = stripped.substring(0, openIdx)
      val afterOpen = stripped.substring(openIdx + 4)
      findTopLevel(afterOpen, ")").map { closeIdx =>
        val condRaw = afterOpen.substring(0, closeIdx)
        val consumed = skipped + openIdx + 4 + closeIdx + 1
        (CondParser.parse(condRaw.trim), parse(exprRaw.trim), consumed)
      }
    }

  private def orConditionalChain(text: String): Option[(Expr, Int)] =
    orConditionalBranch(text).flatMap { (cond1, expr1, len1) =>
      def more(pos: Int, acc: List[(Cond, Expr)]): (List[(Cond, Expr)], Int) =
        if text.substring(pos).startsWith(" or ") then
          orConditionalBranch(text.substring(pos + 4)) match
            case Some((cond, expr, len)) =>
              more(pos + 4 + len, acc :+ (cond -> expr))
            case None => (acc, pos)
        else (acc, pos)
      val (restBranches, endPos) = more(len1, Nil)
      if restBranches.isEmpty then None
      else Some((Conditional((cond1 -> expr1) :: restBranches, None), endPos))
    }

  // "a [=algo|display text=] (of)? |arg|" — a single-argument algorithm
  // invocation phrased as a noun (e.g. "a [=get a copy of the buffer
  // source|copy of the bytes held by the buffer=] |bytes|"), as opposed to
  // LinkProse's "[=algo=] ARGS" verb phrasing. Deliberately anchored to a
  // single *bare variable* argument with nothing else trailing, so phrases
  // like RelativeClauseDesc/SuchThatDesc above (which have more text after
  // the bracket/variable) can't match here even if the guards above it were
  // ever removed. Placed after the more specific "a [=/new=] ..." / "a new,
  // empty ..." construction patterns so it only catches the general case.
  private val LinkIndefVar =
    """(?si)^(?:the|an?)\s+(\[=(?:(?!=\]).)+?=\])\s+(?:of\s+)?(\|[^|]+\|)$""".r

  // ---- Scalars & glossary terms ----

  private val NumberPat = """^\d+(?:\.\d+)?$""".r
  private val HexPat = """^0x[0-9a-fA-F]+$""".r
  // `"{{Dict/member}}"` — a Bikeshed dfn-link to a dictionary/interface
  // member, used (unusually) as a literal ordered-map key rather than in
  // prose (e.g. js-api's «[ "{{WebAssemblyInstantiatedSource/module}}" →
  // |module|, ... ]»). Bikeshed renders this as a hyperlinked "module" — only
  // the member name after the slash is real string content, so this must be
  // tried before the plain QuotedStr below (which would otherwise keep the
  // braces/interface-name literally, producing a key nothing ever looks up).
  private val QuotedBracedMemberLink = """(?s)^"\{\{[^/"}]+/([^"}]+)\}\}"$""".r
  // `"<code>str</code>"` — Bikeshed's markup for a literal string's content
  // (e.g. `DefinePropertyOrThrow(|F|, "<code>prototype</code>", ...)`,
  // webidl/index.bs:11983). Only `str` is real string content, so this must
  // be tried before the plain QuotedStr below, which would keep the tags and
  // define a property nothing ever looks up. Inner whitespace is kept as-is
  // (`"<code>get </code>"` is the `"get "` name prefix).
  private val QuotedCodeStr = """(?s)^"<code>([^"<]*)</code>"$""".r
  private val QuotedStr = """^"([^"]*)"$""".r
  private val EmptyString = """(?i)^the empty string$""".r
  // the value bound by a preceding `Cond.Throws` check ("If this throws an
  // exception, catch it, ... with the exception, ..."); "catch it" itself
  // carries no separate binding, so this is the only place that name needs
  // to resolve to a variable.
  private val TheException = """(?i)^the exception$""".r
  // "a {{RuntimeError}} exception as if a [=trap=] was executed" (js-string
  // builtins, 10 occurrences, index.bs:1924 onward, always `{{RuntimeError}}`)
  // — the exception *type* name is irrelevant: this is spec-author shorthand
  // for "make this call behave exactly like a genuine Core Wasm trap" (see
  // `$callhostfunc`'s own doc, `4.3-execution.instructions.spectec`: the host
  // function's `instr*` result is "the host function's own return
  // value/thrown exception/trap, verbatim, uninterpreted"), not a real
  // JS-catchable `Exception` object crossing the Wasm boundary the way
  // `create_a_host_function`'s own throw path does. Compiles to the same
  // `Case("TRAP", Nil)` a genuine Core Wasm `TRAP` instruction would —
  // `create_a_builtin_function`'s hostfunc wrapper checks for exactly this
  // shape on the abrupt path to decide whether to build a real Wasm trap
  // instead of a `(ref.exn) throw_ref` pair.
  private val TrapException =
    """(?si)^an?\s+\{\{[^}]+\}\}\s+exception as if an?\s+\[=trap=\]\s+was executed$""".r
  private val BoolTrue = """(?i)^true$""".r
  private val BoolFalse = """(?i)^false$""".r
  private val BoldConst = """(?s)^\*\*([^*]+)\*\*$""".r
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

  def normalizeLink(link: String): String =
    link.replaceAll("""\|[^=\]]*(?==\])""", "")

  /** camelCases a captured "X steps" dfn name into the record field storing
    * that closure, e.g. "default method" -> "defaultMethodSteps", "method" ->
    * "methodSteps", "getter" -> "getterSteps" — see
    * [[StepsCallWithArgs]]/[[StepsCallNoArgs]].
    */
  private def stepsFieldName(raw: String): String =
    raw.trim.split("\\s+").toList match
      case head :: tail =>
        (head.toLowerCase :: tail.map(w =>
          w.take(1).toUpperCase + w.drop(1).toLowerCase,
        )).mkString + "Steps"
      case Nil => "steps"

  /** Strips a Bikeshed `{{...}}` IDL-reference wrapper, e.g. a `[[...]]`
    * internal slot named after an intrinsic is conventionally written
    * `[[{{%Promise%}}]]` rather than `[[%Promise%]]`.
    */
  private def stripBraces(s: String): String =
    s.stripPrefix("{{").stripSuffix("}}")

  /** `Field(parse(baseRaw), name)`, where `name` is `link`'s dfn text with its
    * `[=`/`=]` markers stripped and lowercased — shared by [[DotFieldLink]]
    * (`base.[=name=]`) and [[PossessiveAssociation]] (`base's [=name=]`), the
    * two field-access spellings that carry the field name as a dfn link rather
    * than a plain identifier (contrast [[DotField]]'s `base.field`).
    *
    * Lowercased because Bikeshed itself resolves `[=link=]`s against their
    * `<dfn>` case-insensitively, so a use site's casing need not match its own
    * definition's — e.g. index.bs:349 defines `<dfn>Exported GC Object
    * cache</dfn>` but index.bs:1650 links it as `[=exported GC object cache=]`,
    * a mismatch invisible in the rendered spec. Taking the link text as-is here
    * would make that (real, if inconsequential) authoring slip a functional
    * bug: `Initialize.scala`'s `AgentRecord` fields would need to be stored
    * under every use site's own idiosyncratic casing to ever be found again,
    * rather than one canonical spelling. Lowercasing both here and at every
    * field-defining site (`Initialize.scala`, `WjiInterp.scala`,
    * `WasmMemoryBridge.scala`) sidesteps the whole class of mismatch the same
    * way Bikeshed's own resolution does, rather than chasing down each future
    * one-off typo as it's discovered.
    */
  private def fieldFromLink(baseRaw: String, link: String): Expr =
    Field(
      parse(baseRaw),
      normalizeLink(link).stripPrefix("[=").stripSuffix("=]").toLowerCase,
    )

  def parse(raw: String): Expr = parseWith(raw, allowSeqFallback = true)

  /** Same as `parse`, but its own top-level match never falls through to
    * [[parseAsSeq]] — used by [[parseAsSeq]]'s own shrink loop so that a
    * substring it can't match doesn't spawn another full `parseAsSeq` search
    * nested inside the one already running (which would otherwise compound into
    * a combinatorial blowup instead of the intended linear "shrink until
    * something matches"). Every case body below still recurses via `parse`, not
    * this — the distinction has to be "did *this* call's match reach its own
    * tail case", not "is the resulting `Expr` an `Unknown`": a case like
    * `Backticked` legitimately returns whatever its own nested `parse(inner)`
    * call decides, which may itself already be a correctly- resolved `Unknown`
    * for a *smaller* string — that must not be mistaken for *this* call's own
    * match having failed and re-triggered on the wrong (larger) string.
    */
  private def parseCore(s: String): Expr =
    parseWith(s, allowSeqFallback = false)

  private def parseWith(raw: String, allowSeqFallback: Boolean): Expr =
    val s = raw.trim
    s match
      // ---- Wrappers ----
      case AbruptPrefix(check, rest) => Abrupt(check, parse(rest))
      case CreatingLinkGivenCall(link, argsRaw) =>
        AlgoCall(normalizeLink(link), parseArgs(argsRaw))
      case ResultOf(rest)                  => parse(rest)
      case EitherPat(rest)                 => parse(rest)
      case TypeAnnotatedPrefix(term, rest) => TypeAnnotated(term, parse(rest))
      case BracedTermValuePrefix(rest)     => parse(rest)
      case BracedTermValueOnly(term)       => SpecTerm(term)
      case BacktickedQuotedStr(v)          => SpecTerm(v)
      case Backticked(inner)               => parse(inner)

      // ---- Closures ----
      case VariadicStepsClosurePrefix(v) =>
        FollowingSteps(List(v), variadicLast = true)
      case StepsClosurePrefix(paramsRaw) =>
        FollowingSteps(
          PipeVarInline.findAllMatchIn(paramsRaw).map(_.group(1)).toList,
        )
      case QueueTaskClosureSuffix() => FollowingSteps(Nil)
      case BareStepsClosure()       => FollowingSteps(Nil)
      case WhichPerformsStepsClosureWithState(stateVar, argsVar) =>
        FollowingSteps(List(stateVar, argsVar))
      case WhichPerformsStepsClosure(paramsRaw) =>
        FollowingSteps(
          PipeVarInline.findAllMatchIn(paramsRaw).map(_.group(1)).toList,
        )
      case PerformingClosureCall(rest)
          if findTopLevel(rest, " given ").isDefined =>
        closureCall(rest)
      case RunningStepsCall(rest) if parseStepsCall(rest).isDefined =>
        parseStepsCall(rest).get
      case RunningClosureCall(rest)
          if parse(rest).isInstanceOf[FollowingSteps] =>
        ClosureCall(parse(rest), Nil)

      // ---- Construction ----
      case RangePrefix(rest) if findTopLevel(rest, " to ").isDefined =>
        val i = findTopLevel(rest, " to ").get
        Range(parse(rest.substring(0, i)), parse(rest.substring(i + 4)))
      case NewExpr(iface, realmRaw) =>
        Link("new", List(SpecTerm(iface), parse(realmRaw)))
      case NewExceptionExpr(iface)    => New(iface)
      case EmptyList()                => List_(Nil)
      case NewByteSeqOfLength(lenRaw) => NewByteSequence(parse(lenRaw))
      case NewArrayBufferWithSlots()  => NewArrayBuffer
      case PlainNewExpr()             => UnknownNew(s)
      case EmptyMapProse()            => Map_(Nil)
      // "[=comp-type/func=] |parameters| → |results|" (destructuring, a
      // `Let` LHS) / "[=comp-type/func=] |wasmParameters| → « »"
      // (construction, an ordinary expression building a fresh functype
      // value) — see `CompTypeArrowPrefix`'s own doc above. Both directions
      // parse identically here (`parse` handles whatever's on each side —
      // bare `|var|`, `«...»` list literal, or `<var ignore>X</var>` — the
      // same way regardless of position); what differs downstream is only
      // `ExpandDestructuringLetPass`'s Let-LHS handling. Uses `findTopLevel`/
      // `splitTopLevel` (bracket-depth-aware, tracking `«»` alongside
      // `()[]{}` — see `TextSplit`) rather than a plain regex specifically so
      // this is safe even when a side is itself a `«...»` list literal (e.g.
      // `« [=externref=] » → « »`): a naive `^«...»$`-anchored regex, tried
      // on the *whole* string, would capture clean through the first group's
      // closing `»` to the *last* one instead (confirmed empirically — this
      // used to silently mis-parse `get_the_javascript_exception_tag`'s
      // `tag_alloc` argument into a bare 1-element list instead of a real
      // functype, with no visible error) — moot here regardless, since the
      // required `[=comp-type/func=] ` prefix means `ListLiteral`/
      // `MapLiteral` below never even attempt this string (neither starts
      // with `«`), but kept for the same reason any top-level split in this
      // file uses `TextSplit`: correctness shouldn't depend on what the
      // *content* of either side happens to look like.
      case FuncExternType(inner) => Case("FUNC", List(parse(inner)))
      case _ if s.startsWith(CompTypeArrowPrefix) =>
        val rest = s.substring(CompTypeArrowPrefix.length)
        splitTopLevel(rest, " → ") match
          case Some((leftRaw, rightRaw)) =>
            Case("[=comp-type/func=]", List(parse(leftRaw), parse(rightRaw)))
          case None => Unknown(s)
      case MapLiteral(inner) =>
        val entries = splitComma(inner).map { e =>
          splitTopLevel(e, " → ") match
            case Some((k, v)) => (parse(k), parse(v))
            case None         => (Unknown(e), Unknown(""))
        }
        Map_(entries)
      case ListLiteral(inner) =>
        List_(splitComma(inner).map(parse))
      case TuplePat(inner)      => Tuple(splitComma(inner).map(parse))
      case AngleTuplePat(inner) => Tuple(splitComma(inner).map(parse))
      case RecordLitPrefix(tname, inner) =>
        parseRecordFields(inner) match
          case Some(fields) => RecordLit(tname, fields)
          case None         => Unknown(s)

      // ---- Call syntax ----
      case JSCallFull(name, argsRaw) =>
        JSCall(name, splitComma(argsRaw).map(parse))
      case AbstractOpCallFull(name, argsRaw) =>
        JSCall(name, splitComma(argsRaw).map(parse))
      case AssociatedWithAttrPat(callRaw, attr) =>
        parse(callRaw) match
          case JSCall(name, args)   => JSCall(name, args :+ Str(attr))
          case AlgoCall(link, args) => AlgoCall(link, args :+ Str(attr))
          case _                    => Unknown(callRaw)
      case PassingToCall(argsRaw, link) =>
        AlgoCall(normalizeLink(link), parseArgs(argsRaw))
      case LinkPassingCall(link, argsRaw) =>
        AlgoCall(normalizeLink(link), parsePassingArgs(argsRaw))
      case LinkFull(link, argsRaw) =>
        normalizeLink(link).stripPrefix("[=").stripSuffix("=]") match
          // "[=𝔽=](x)"/"[=ℤ=](x)"/"[=ℝ=](x)" — ECMA-262's Number/BigInt/
          // mathematical-value notation (𝔽/ℤ: math value -> Number/BigInt; ℝ:
          // the inverse, reusing AsMath) special-cased ahead of the generic
          // AlgoCall fallback, so these three never resolve against a function
          // literally named 𝔽/ℤ/ℝ (mainline ESMeta's own separate parser
          // special-cases 𝔽/ℤ the same way, `esmeta.lang.util.Parser`).
          case "𝔽" => AsNumber(parse(argsRaw))
          case "ℤ"  => AsBigInt(parse(argsRaw))
          case "ℝ"  => AsMath(parse(argsRaw))
          case _ =>
            AlgoCall(normalizeLink(link), splitComma(argsRaw).map(parse))
      case LinkOfForIn(link, subjRaw, realmRaw) =>
        Link(normalizeLink(link), List(parse(subjRaw), parse(realmRaw)))
      case LinkOfInheritedInterfaceIn(link, realmRaw) =>
        Link(
          normalizeLink(link),
          List(Field(Var("interface"), "inherit"), parse(realmRaw)),
        )
      case LinkOfWithIdentifierIn(link, varRaw, realmRaw) =>
        val v = parse(varRaw)
        Link(normalizeLink(link), List(v, Field(v, "id"), parse(realmRaw)))
      case LinkProse(link, prose) =>
        Link(normalizeLink(link), parseArgs(prose))
      case LinkOnly(link) => Link(normalizeLink(link), Nil)
      case TrailingLinkCall(valueRaw, link) =>
        Link(normalizeLink(link), List(parse(valueRaw)))

      // ---- Bare references ----
      case ThisOnly()       => This
      case ThisValue()       => This
      case PassedArguments() => ArgumentsList
      case NewTargetOnly()   => NewTarget
      case PronounOnly()     => Pronoun
      case GivenValueOnly()  => GivenValue
      case VarOnly(name)     => Var(name)
      case VarIgnore(name)   => Var(name.trim)

      // tried before "---- Arithmetic & casts ----" below, unlike every other
      // structural-access pattern (e.g. ElementAt further down) -- its own
      // index sub-expression can itself be a "+"-expression ("Set the
      // element at index |start| + |i| in |array| to ...", js-string's
      // intoCharCodeArray), so the generic top-level-BinOp fallback must not
      // get a chance to split the whole reference apart first.
      case ElementAtRef(idx, arr) => Index(parse(arr), parse(idx))

      // ---- Arithmetic & casts ----
      case _ if findLastTopLevelAny(s, BinOpSeps).isDefined =>
        val (i, sep) = findLastTopLevelAny(s, BinOpSeps).get
        BinOp(
          parse(s.substring(0, i)),
          parseBOp(sep),
          parse(s.substring(i + sep.length)),
        )
      case AsMathPat(inner)                     => AsMath(parse(inner))
      case AsWasmPat(inner, ty)                 => AsWasm(parse(inner), ty)
      case RoundedToNearestRepresentable(inner) => parse(inner)
      case PowPat(base, exp)                    => Pow(parse(base), parse(exp))
      case NegPat(inner)                        => Neg(parse(inner))

      // ---- Structural access ----
      case SlotMethodCall(baseRaw, slot, argsRaw) =>
        MethodCall(
          parse(baseRaw),
          stripBraces(slot),
          splitComma(argsRaw).map(parse),
        )
      case SlotAccess(baseRaw, slot) => Field(parse(baseRaw), stripBraces(slot))
      case AnchorLink(anchor)        => Link(anchor, Nil)
      case BareSlotName(slot)        => Str(stripBraces(slot))
      case PossessiveSlot(baseRaw, slot) =>
        Field(parse(baseRaw), stripBraces(slot))
      case TheSlotOf(slot, baseRaw) => Field(parse(baseRaw), stripBraces(slot))
      case DotFieldLink(baseRaw, link) => fieldFromLink(baseRaw, link)
      case DotField(baseRaw, field)    => Field(parse(baseRaw), field)
      case LengthOf(inner)             => Length(parse(inner))
      case ElementCount(inner)         => Length(parse(inner))
      case PossessiveSize(inner)       => Length(parse(inner))
      case PrefixedWith(baseRaw, prefixRaw) =>
        Concat(List(parse(prefixRaw), parse(baseRaw)))
      case ConcatenationOf(firstRaw, secondRaw) =>
        Concat(List(parse(firstRaw), parse(secondRaw)))
      case ElementAt(idx, arr)    => Index(parse(arr), parse(idx))
      case IndexOfPat(list, elem) => IndexOf(parse(list), parse(elem))
      case ShortestArgumentListOfEntries(baseRaw) =>
        ShortestArgumentList(parse(baseRaw))
      case StringPrependedTo(prefixRaw, restRaw) =>
        Concat(List(parse(prefixRaw), parse(restRaw)))
      case PossessiveIdentifier(baseRaw) => Field(parse(baseRaw), "id")
      case PossessiveType(varRaw)        => Field(parse(varRaw), "ty")
      case IdentifierOfType(varRaw)      => Field(parse(varRaw), "id")
      case InterfaceInheritsFrom(varRaw) => Field(parse(varRaw), "inherit")
      case InterfaceTypeValueOf(varRaw)  => parse(varRaw)
      case IsGlobalPrototypeChainMutable(baseRaw) =>
        Field(parse(baseRaw), "is global prototype chain mutable")
      case RealmGlobalObject(baseRaw) => Field(parse(baseRaw), "GlobalObject")
      case AssociatedRealm(baseRaw)   => Field(parse(baseRaw), "Realm")
      case MemberOfDefinition(unforgeable, kind, baseRaw) =>
        val memberKind = kind match
          case "regular attributes" => MemberKind.RegularAttribute
          case "static attributes"  => MemberKind.StaticAttribute
          case "regular operations" => MemberKind.RegularOperation
          case "static operations"  => MemberKind.StaticOperation
          case _                    => ???
        GetMember(parse(baseRaw), memberKind, unforgeable != null)
      case PossessiveAssociation(baseRaw, link) => fieldFromLink(baseRaw, link)
      case IndexByStr(baseRaw, key)    => Index(parse(baseRaw), Str(key))
      case IndexByVar(baseRaw, varRaw) => Index(parse(baseRaw), parse(varRaw))
      case IndexByNum(baseRaw, n)      => Index(parse(baseRaw), parse(n))
      case IndexByExpr(baseRaw, idx)   => Index(parse(baseRaw), parse(idx))

      // ---- Noun-phrase descriptions ----
      case DataBlockIdentifiedWith(memaddrRaw) => DataBlockOf(parse(memaddrRaw))
      case RelativeClauseDesc(link, desc) =>
        Described(normalizeLink(link), desc.trim)
      case OfTypeGeneric(typeArg) => SpecTerm(typeArg)
      case ForConstructors()      => SpecTerm("Constructor")
      case SuchThatDesc(desc, cond) =>
        SuchThat(desc.trim, CondParser.parse(cond.trim))
      case _ if conditionalParts(s).isDefined =>
        val (thenRaw, condRaw, elseRaw) = conditionalParts(s).get
        Conditional(
          List(CondParser.parse(condRaw) -> parse(thenRaw)),
          Some(parse(elseRaw)),
        )
      case LinkIndefVar(link, arg) =>
        Link(normalizeLink(link), List(parse(arg)))

      // ---- Scalars & glossary terms ----
      case NumberPat()                    => Num(s)
      case HexPat()                       => Num(s)
      case QuotedBracedMemberLink(member) => Str(member)
      case QuotedCodeStr(v)               => Str(v)
      case QuotedStr(v)                   => Str(v)
      case EmptyString()                  => Str("")
      case TheException()                 => Var("exception")
      case TrapException()                => Case("TRAP", Nil)
      case BoolTrue()                     => Bool(true)
      case BoolFalse()                    => Bool(false)
      case BoldConst(_)                   => SpecTerm(s)
      case SpecTermPat()                  => SpecTerm(s)
      case EmuConst(v)                    => SpecTerm(v)
      case EmuVal(v)                      => SpecTerm(v)
      case BracedTerm(inner)              => SpecTerm(inner)
      case BracedFields(inner) => parseBracedFields(inner).getOrElse(Unknown(s))
      case CrossSpecRef(text)  => SpecTerm(text)
      case RealmSettingsObject() => SpecTerm("realm/settings object")

      case _ =>
        if allowSeqFallback then parseAsSeq(s).getOrElse(Unknown(s))
        else Unknown(s)

  /** Last-resort fallback for `parse`'s final case: every more specific case
    * above has already failed, so try to decompose the *whole* string into a
    * run of individually parseable `Expr`s with nothing left over (a
    * longest-match-first tokenizing loop like [[parseArgs]] uses below, except
    * both ends of each candidate span are anchored to word/clause boundaries —
    * a space on either side, or a string edge — rather than shrinking one
    * character at a time, since a partial-word span could never usefully match
    * anyway) and wrap the result in [[Expr.Seq_]] instead of falling all the
    * way to [[Unknown]]. Returns `None` (never a partial `Seq_`) if any part of
    * the string can't be matched, or if fewer than two `Expr`s result (a lone
    * match here would just be `parse` itself finding what an earlier, more
    * specific case should have — not genuine juxtaposed-field structure worth
    * preserving) — a partial or spurious guess would be worse than the honest
    * "not yet mechanized" signal this is a fallback *from*, not a
    * general-purpose alternate parse path (see `parseUntaggedForm`'s own doc
    * for a confirmed instance of exactly this kind of over-eager fallback
    * silently swallowing a genuine multi-arg call when it was tried as a
    * general `parse` case before).
    */
  private def parseAsSeq(s: String): Option[Expr] =
    val n = s.length
    if n == 0 then None
    else
      val starts = (0 until n).filter(i => i == 0 || s(i - 1) == ' ').toArray
      // A candidate end must line up with the *next* token's start (not
      // "right before the following space") — `parseCore` trims its input,
      // so a span that includes the separating space still matches, and only
      // landing exactly on a `starts` position keeps `consumedTo` in sync
      // with the next `from` below.
      val ends = (starts :+ n).distinct
      val result = collection.mutable.ListBuffer[Expr]()
      var si = 0
      var consumedTo = 0
      var stuck = false
      while si < starts.length && !stuck do
        val from = starts(si)
        if from != consumedTo then stuck = true
        else
          val candidates = ends.filter(_ > from)
          var idx = candidates.length - 1
          var found = false
          while idx >= 0 && !found do
            val to = candidates(idx)
            parseCore(s.substring(from, to)) match
              case Unknown(_) => idx -= 1
              case expr =>
                result += expr
                consumedTo = to
                si = starts.indexWhere(_ >= to)
                if si < 0 then si = starts.length
                found = true
          if !found then stuck = true
      if !stuck && consumedTo == n && result.size >= 2 then
        Some(Seq_(result.toList))
      else None

  /** Extracts argument [[Expr]]s from a prose string.
    *
    * Start positions are restricted to word boundaries (position 0 and every
    * position right after a space). End positions shrink one character at a
    * time so trailing punctuation is trimmed naturally without pre-processing.
    *
    * At each start position, `orConditionalChain` is tried first, as a bounded
    * prefix match — see its own doc for why this can't just be an ordinary
    * `parse` case tried by the generic shrinking loop below.
    */
  private[wji] def parseArgs(prose: String): List[Expr] =
    val s = prose.trim
    val n = s.length
    val starts = (0 until n).filter(i => i == 0 || s(i - 1) == ' ').toArray
    val result = collection.mutable.ListBuffer[Expr]()
    var si = 0
    while si < starts.length do
      val from = starts(si)
      orConditionalChain(s.substring(from)) match
        case Some((expr, consumed)) =>
          result += expr
          val to = from + consumed
          si = starts.indexWhere(_ >= to)
          if si < 0 then si = starts.length
        case None =>
          var to = n
          var found = false
          while to > from && !found do
            parseCore(s.substring(from, to)) match
              case Unknown(_) => to -= 1
              case expr =>
                result += expr
                si = starts.indexWhere(_ >= to)
                if si < 0 then si = starts.length
                found = true
          if !found then si += 1
    result.toList

  // A single positional component of an untagged multi-var form: either a
  // bare `|var|`, or `<var ignore>X</var>` for a component the surrounding
  // prose never refers to again (e.g. `GetGlobalValue`'s "<var
  // ignore>mut</var> |valuetype|", index.bs:1212 — contrast the `Let`-LHS
  // form below, where every component is always bound and so always a bare
  // `|var|`).
  private val UntaggedFormComponent =
    """\|[^|]+\||<var\s+ignore>[^<]*</var>""".r
  // "|mut| |valuetype|" / "<var ignore>mut</var> |valuetype|" — SpecTec's own
  // AL `CaseE` supports an empty mixop for a syntax rule with no keyword
  // tokens of its own (Wasm Core's `globaltype ::= mut valtype`): N ≥ 2
  // components named positionally, separated by nothing but whitespace — no
  // tag/keyword between them the way the comptype arrow's "[=comp-type/func=]"
  // or a `Cond.IsOfForm`'s "REF ..." has one. Requires 2+ components so it doesn't
  // also swallow `VarOnly`'s single-`|var|` case.
  private val UntaggedForm =
    ("""(?s)^(?:""" + UntaggedFormComponent.regex + """)""" +
      """(?:\s+(?:""" + UntaggedFormComponent.regex + """))+$""").r

  /** Untagged-form-only entry point — used for a `Let` LHS (`InstrParser`) and
    * a `Cond.IsOfForm`'s "form" text (`CondParser`), never reachable from
    * general [[parse]] itself, even though it builds a plain [[Case]] the same
    * way the comptype arrow case does: `parseArgs`'s tokenizer above greedily
    * tries the *longest* parseable prefix at each word boundary, so if this
    * shape were reachable from general `parse`, a genuine `Case`'s own
    * multi-arg list written the same bare, space-separated way (e.g. "[=ref=]
    * |null| |heaptype|", parsed via `parseArgs`) would get swallowed into one
    * nested `Case("", [Var(null), Var(heaptype)])` instead of staying two
    * separate top-level args — confirmed by `SnapshotSpec` regressing
    * `ToWebAssemblyValue`'s `ref` case exactly this way when this was first
    * tried as a general `parse` case. `Case("", ...)` mirrors SpecTec's empty
    * mixop directly rather than inventing a separate node —
    * `ExpandDestructuringLetPass`/`ExpandIsOfFormPass` both treat an empty tag
    * as "always matches, nothing to assert" rather than special-casing it.
    */
  private[wji] def parseUntaggedForm(raw: String): Expr =
    val trimmed = raw.trim
    if UntaggedForm.matches(trimmed) then
      Case(
        "",
        UntaggedFormComponent
          .findAllMatchIn(trimmed)
          .map(m => parse(m.matched))
          .toList,
      )
    else parse(trimmed)
