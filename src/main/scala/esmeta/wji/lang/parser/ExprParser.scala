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
  // "a [=term=] which performs the following steps when called with
  // argument(s) |V|[, ...]:" — must precede RelativeClauseDesc, which would
  // otherwise swallow it into an unevaluable Described.
  private val WhichPerformsStepsClosure =
    """(?si)^an?\s+\[=[^\]]+=\]\s+which\s+performs\s+the\s+following\s+steps\s+when\s+called\s+with\s+(?:arguments?\s+)?(\|[^|]+\|(?:\s*(?:,|and)\s*\|[^|]+\|)*)\s*:?\s*$""".r
  // "a [=term=] which performs the following steps when called with state
  // |S| and arguments |V|:" — SpecPatch-authored (`create a host function`,
  // #28), a leading named `state` parameter ahead of the usual variadic
  // `arguments` one. Tried before WhichPerformsStepsClosure above (more
  // specific — that pattern's leading `(?:arguments?\s+)?` only tolerates
  // the literal word "argument(s)" before the first `|var|`, not "state").
  private val WhichPerformsStepsClosureWithState =
    """(?si)^an?\s+\[=[^\]]+=\]\s+which\s+performs\s+the\s+following\s+steps\s+when\s+called\s+with\s+state\s+\|([^|]+)\|\s+and\s+arguments\s+\|([^|]+)\|\s*:?\s*$""".r
  // "performing CLOSURE[,] given ARG[, ARG...]" — invoking a closure *value*
  // (contrast with the four "the following steps ...:" forms above, which
  // *define* one), e.g. "the result of performing |onFullfilledStepsArg|
  // given |value|" (patched PromiseReactionJob text, see SpecPatch). CLOSURE
  // is parsed as a general Expr (not just a bare |var|) since nothing about
  // the phrasing restricts it to a variable reference.
  private val PerformingClosureCall = """(?si)^performing\s+(.+)$""".r
  private val PipeVarInline = """\|([^|]+)\|""".r

  // ---- Construction: builds a new composite value ----

  // "[=the range=] LOW to HIGH" — the leading link text varies ("the range",
  // "range", ...) but always mentions "range". Both bounds are assumed
  // inclusive (see [[Expr.Range]]), so a trailing ", inclusive" is left for
  // the caller to strip along with the rest of the sentence. Must precede
  // LinkProse below, which would otherwise misparse this as a call.
  private val RangePrefix = """(?is)^\[=[^=]*range[^=]*=\]\s+(.+)$""".r
  private val NewExpr =
    """(?si)^a\s+\[=/new=\]\s+\{\{([^}]+)\}\}(?:\s+object)?$""".r
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
  private val NewExceptionExpr =
    """(?si)^an?\s+\{\{([^}]+)\}\}(?:\s+exception)?$""".r
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

  // ---- Call syntax: explicit invocation of a named algorithm/AO ----

  private val JSCallFull = """(?s)^\[\$([^\$]+)\$\]\((.*)\)$""".r
  // unlike LinkProse/LinkOnly below, the explicit `(...)` here is
  // unambiguous call syntax (mirrors JSCallFull) — no term/value
  // reference is ever written this way — so this can go straight to
  // AlgoCall without waiting for ResolveLinksPass.
  private val LinkFull = """(?s)^(\[=[^\]]+\])\s*\((.*)\)$""".r
  private val LinkProse = """(?s)^(\[=[^\]]+\])\s+(.+)$""".r
  private val LinkOnly = """(?s)^(?:the\s+)?(\[=[^\]]+\])$""".r
  // "VALUE, [=link=]" — spec's passive-voice idiom for a unary conversion
  // applied to the value stated just before it (e.g. "|result|,
  // [=converted to a JavaScript value=]", "|map|'s [=map/size=], [=converted
  // to a JavaScript value=]." — both from webidl/index.bs). Mirrors
  // DotFieldLink's `base.[=name=]` shape but with a comma rather than a dot;
  // unlike DotFieldLink (a field read), this is a call, so it produces a
  // Link (resolved to AlgoCall/Case by ResolveLinksPass), not Field. Its
  // trailing separator (",") is disjoint from every other call/field-access
  // pattern's own trailing separator, so it can go anywhere in this list.
  private val TrailingLinkCall = """(?s)^(.+),\s*(\[=[^\]]+\])$""".r

  // ---- Bare references ----

  private val ThisOnly = """(?s)^\*\*this\*\*$""".r
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
  private val PowPat = """(?s)^(\d+)<sup>(.+?)</sup>$""".r
  private val NegPat = """(?s)^[-−](.+)$""".r

  // ---- Structural access: field/slot/index/length/association off a base
  // value ----

  private val SlotAccess = """(?s)^(.+)\.\\?\[\[([^\]]+)\]\]$""".r
  // a bare "\[[SlotName]]" with no base — the slot's *name*, used as a value
  // rather than read off a specific object (e.g. CreateBuiltinFunction's
  // `additionalInternalSlotsList` argument, « \[[FunctionAddress]] »).
  // Mirrors mainline `esmeta.compiler.Compiler`'s own `InternalSlots` XRef
  // handling, which likewise compiles a bare slot name to `EStr`.
  private val BareSlotName = """(?s)^\\?\[\[([^\]]+)\]\]$""".r
  private val PossessiveSlot =
    """(?si)^the value of (.+)'s \\?\[\[([^\]]+)\]\] internal slot$""".r
  // e.g. "|module|.[=imports=]" — a WebAssembly-spec record field written
  // with a dot, where the `[=...=]` is a documentation link on the field
  // name rather than a call (contrast with `LinkFull`/`LinkProse`,
  // which require the string to *start* with `[=`).
  private val DotFieldLink = """(?s)^(.+)\.(\[=[^\]]+\])$""".r
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
  // must precede PossessiveAssociation below — "the X's [=list/size=]"
  // would otherwise also match its more general "'s [=link=]" shape.
  private val PossessiveSize = """(?si)^(.+)'s \[=list/size=\]$""".r
  private val ElementAt =
    """(?si)^the value of the element stored at index (.+) in (.+)$""".r
  // "the index of LIST where ELEM is found" (index.bs:1255) — see
  // Expr.IndexOf / ExpandIndexOfPass.
  private val IndexOfPat =
    """(?si)^the index of (.+) where (.+) is found$""".r
  // "the [=list=] of [=regular operations=] that are [=members=] of |X|" —
  // webidl/index.bs's `define the regular operations`/`define the
  // operations` member-list projection. `esmeta.wji.Initialize` builds each
  // WJI `Definition`'s operations directly as an `.operations` field on the
  // record it hands to `create_a_namespace_object`, so this reduces to a
  // plain field read — no separate interface/attribute/member record model
  // needed for WJI's namespace-only reachable scope. Lowercase, matching the
  // interface/namespace-member field naming convention (see
  // [[fieldFromLink]]).
  private val MemberOfDefinition =
    """(?si)^the \[=list=\] of \[=([^\[]+)=\] that are \[=members=\] of (.+)$""".r
  // "|op|'s [=identifier=]" — webidl/index.bs's dfn for an operation's/
  // attribute's name; `esmeta.wji.Initialize.seedHostDefined`'s
  // `operationRecord`/`attributeRecord` both store this under the literal
  // field name `id` (matching `esmeta.wji.lang`'s own `WjiOperation`/
  // `WjiAttribute.id`), not `identifier` — so, like AssociatedRealm below,
  // this needs its own mapping rather than falling through to the generic
  // `fieldFromLink` (which would read the dfn text as-is). Must precede
  // PossessiveAssociation below for the same reason AssociatedRealm does.
  private val PossessiveIdentifier = """(?si)^(.+)'s \[=identifier=\]$""".r
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
    """(?si)^the (.+)'s (?:associated )?(\[=[^\]]+\])$""".r
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
  private val TrailingOtherwise = """(?i)\s+otherwise$""".r
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
    """(?si)^(?:the|an?)\s+(\[=[^\]]+\])\s+(?:of\s+)?(\|[^|]+\|)$""".r

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
  private val QuotedStr = """^"([^"]*)"$""".r
  private val EmptyString = """(?i)^the empty string$""".r
  // the value bound by a preceding `Cond.Throws` check ("If this throws an
  // exception, catch it, ... with the exception, ..."); "catch it" itself
  // carries no separate binding, so this is the only place that name needs
  // to resolve to a variable.
  private val TheException = """(?i)^the exception$""".r
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

  /** Strips a Bikeshed `{{...}}` IDL-reference wrapper, e.g. a `[[...]]`
    * internal slot named after an intrinsic is conventionally written
    * `[[{{%Promise%}}]]` rather than `[[%Promise%]]`.
    */
  private def stripBraces(s: String): String =
    s.stripPrefix("{{").stripSuffix("}}")

  /** `Field(parse(baseRaw), name)`, where `name` is `link`'s dfn text with its
    * `[=`/`=]` markers stripped — shared by [[DotFieldLink]] (`base.[=name=]`)
    * and [[PossessiveAssociation]] (`base's [=name=]`), the two field-access
    * spellings that carry the field name as a dfn link rather than a plain
    * identifier (contrast [[DotField]]'s `base.field`).
    */
  private def fieldFromLink(baseRaw: String, link: String): Expr =
    Field(
      parse(baseRaw),
      normalizeLink(link).stripPrefix("[=").stripSuffix("=]"),
    )

  def parse(raw: String): Expr =
    val s = raw.trim
    s match
      // ---- Wrappers ----
      case AbruptPrefix(check, rest)   => Abrupt(check, parse(rest))
      case ResultOf(rest)              => parse(rest)
      case EitherPat(rest)             => parse(rest)
      case TypeAnnotatedPrefix(rest)   => parse(rest)
      case BracedTermValuePrefix(rest) => parse(rest)
      case BracedTermValueOnly(term)   => SpecTerm(term)
      case BacktickedQuotedStr(v)      => SpecTerm(v)
      case Backticked(inner)           => parse(inner)

      // ---- Closures ----
      case VariadicStepsClosurePrefix(v) =>
        FollowingSteps(List(v), variadicLast = true)
      case StepsClosurePrefix(paramsRaw) =>
        FollowingSteps(
          PipeVarInline.findAllMatchIn(paramsRaw).map(_.group(1)).toList,
        )
      case QueueTaskClosureSuffix() => FollowingSteps(Nil)
      case WhichPerformsStepsClosureWithState(stateVar, argsVar) =>
        FollowingSteps(List(stateVar, argsVar))
      case WhichPerformsStepsClosure(paramsRaw) =>
        FollowingSteps(
          PipeVarInline.findAllMatchIn(paramsRaw).map(_.group(1)).toList,
        )
      case PerformingClosureCall(rest)
          if findTopLevel(rest, " given ").isDefined =>
        val (closureRaw, argsRaw) = splitTopLevel(rest, " given ").get
        ClosureCall(
          parse(closureRaw.stripSuffix(",")),
          splitComma(argsRaw.trim.replaceFirst("""(?i)^arguments?\s+""", ""))
            .map(parse),
        )

      // ---- Construction ----
      case RangePrefix(rest) if findTopLevel(rest, " to ").isDefined =>
        val i = findTopLevel(rest, " to ").get
        Range(parse(rest.substring(0, i)), parse(rest.substring(i + 4)))
      case NewExpr(iface)             => New(iface)
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

      // ---- Call syntax ----
      case JSCallFull(name, argsRaw) =>
        JSCall(name, splitComma(argsRaw).map(parse))
      case AssociatedWithAttrPat(callRaw, attr) =>
        parse(callRaw) match
          case JSCall(name, args)   => JSCall(name, args :+ Str(attr))
          case AlgoCall(link, args) => AlgoCall(link, args :+ Str(attr))
          case _                    => Unknown(callRaw)
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
      case LinkProse(link, prose) =>
        Link(normalizeLink(link), parseArgs(prose))
      case LinkOnly(link) => Link(normalizeLink(link), Nil)
      case TrailingLinkCall(valueRaw, link) =>
        Link(normalizeLink(link), List(parse(valueRaw)))

      // ---- Bare references ----
      case ThisOnly()       => This
      case GivenValueOnly() => GivenValue
      case VarOnly(name)    => Var(name)
      case VarIgnore(name)  => Var(name.trim)

      // ---- Arithmetic & casts ----
      case _ if findLastTopLevelAny(s, BinOpSeps).isDefined =>
        val (i, sep) = findLastTopLevelAny(s, BinOpSeps).get
        BinOp(
          parse(s.substring(0, i)),
          parseBOp(sep),
          parse(s.substring(i + sep.length)),
        )
      case AsMathPat(inner)     => AsMath(parse(inner))
      case AsWasmPat(inner, ty) => AsWasm(parse(inner), ty)
      case PowPat(base, exp)    => Pow(parse(base), parse(exp))
      case NegPat(inner)        => Neg(parse(inner))

      // ---- Structural access ----
      case SlotAccess(baseRaw, slot) => Field(parse(baseRaw), stripBraces(slot))
      case BareSlotName(slot)        => Str(stripBraces(slot))
      case PossessiveSlot(baseRaw, slot) =>
        Field(parse(baseRaw), stripBraces(slot))
      case DotFieldLink(baseRaw, link)   => fieldFromLink(baseRaw, link)
      case DotField(baseRaw, field)      => Field(parse(baseRaw), field)
      case LengthOf(inner)               => Length(parse(inner))
      case ElementCount(inner)           => Length(parse(inner))
      case PossessiveSize(inner)         => Length(parse(inner))
      case ElementAt(idx, arr)           => Index(parse(arr), parse(idx))
      case IndexOfPat(list, elem)        => IndexOf(parse(list), parse(elem))
      case PossessiveIdentifier(baseRaw) => Field(parse(baseRaw), "id")
      case AssociatedRealm(baseRaw)      => Field(parse(baseRaw), "Realm")
      case MemberOfDefinition(kind, baseRaw) =>
        val memberKind = kind match
          case "regular attributes" => MemberKind.RegularAttribute
          case "static attributes"  => MemberKind.StaticAttribute
          case "regular operations" => MemberKind.RegularOperation
          case "static operations"  => MemberKind.StaticOperation
          case _                    => ???
        GetMember(parse(baseRaw), memberKind)
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
      case SuchThatDesc(desc, cond) =>
        SuchThat(desc.trim, CondParser.parse(cond.trim))
      case _ if conditionalParts(s).isDefined =>
        val (thenRaw, condRaw, elseRaw) = conditionalParts(s).get
        Conditional(CondParser.parse(condRaw), parse(thenRaw), parse(elseRaw))
      case LinkIndefVar(link, arg) =>
        Link(normalizeLink(link), List(parse(arg)))

      // ---- Scalars & glossary terms ----
      case NumberPat()                    => Num(s)
      case HexPat()                       => Num(s)
      case QuotedBracedMemberLink(member) => Str(member)
      case QuotedStr(v)                   => Str(v)
      case EmptyString()                  => Str("")
      case TheException()                 => Var("exception")
      case BoolTrue()                     => Bool(true)
      case BoolFalse()                    => Bool(false)
      case BoldConst(_)                   => SpecTerm(s)
      case SpecTermPat()                  => SpecTerm(s)
      case EmuConst(v)                    => SpecTerm(v)
      case EmuVal(v)                      => SpecTerm(v)
      case BracedTerm(inner)              => SpecTerm(inner)
      case CrossSpecRef(text)             => SpecTerm(text)
      case RealmSettingsObject()          => SpecTerm("realm/settings object")

      case _ => Unknown(s)

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
