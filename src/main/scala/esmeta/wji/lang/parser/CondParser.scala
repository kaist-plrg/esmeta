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
  // "EXPR is the {{X}} [=interface=]" — identity against one *specific* named
  // interface (webidl/index.bs:12057's "Otherwise, if |interface| is the
  // {{DOMException}} [=interface=], then set |proto| to ..."), unlike
  // ArticleLink's "EXPR is a(n) [=interface=]" (kind-membership: is this *some*
  // interface at all — see `ExpandWjiIsTypePass`'s `memberKindOf("interface")`).
  // X is taken from the `{{...}}` IDL-name link, not the `[=interface=]` dfn
  // link itself (which only names the noun being checked, always the literal
  // text "interface" at every real call site), so it parses to the same
  // `IsType` node as ArticleLink but keyed by the specific interface name
  // instead of the generic noun — `ExpandWjiIsTypePass` is what later decides
  // whether that name is one it can resolve to a real identity check.
  private val IsTheBracedInterfaceLink =
    """(?si)^the\s+\{\{([^}]+)\}\}\s+\[=interface=\]$""".r
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
  // "EXPR [=implements=] {{Iface}}" / "EXPR does not [=implement=] {{Iface}}"
  // — RHS is either a literal `{{Iface}}` (the corpus's only real shape
  // today) or a `|variable|` (webidl/index.bs's own general form, e.g. "|O|
  // is an object that [=implements=] |I|"). Captured generally and handed
  // straight to `ExprParser.parse`, which already resolves each shape on its
  // own (`{{X}}` -> `SpecTerm(X)` via `BracedTerm`, `|X|` -> `Var(X)` via
  // `VarOnly`) — no special-casing needed here.
  private val ImplementsPos =
    """(?si)^(.*?)\s+\[=implements=\]\s+(\{\{[^}]+\}\}|\|[^|]+\|)$""".r
  private val ImplementsNeg =
    """(?si)^(.*?)\s+does not \[=implement=\]\s+(\{\{[^}]+\}\}|\|[^|]+\|)$""".r
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
  // "X is [not] declared with a/the [{{Y}}] [=extended attribute=]" —
  // webidl_yet_categorized.md category II-A (e.g. "|interface| is declared
  // with the [{{Global}}] [=extended attribute=]", "|operation| is declared
  // with a [{{Default}}] [=extended attribute=]"). `Initialize.scala` seeds
  // every interface/operation/attribute record's `extendedAttributes` field
  // as a *list* of `{id, value}` records (mirroring the declared IDL shape
  // 1:1 — extended attributes are literally written as a comma-separated
  // list in `[...]` syntax, and WebIDL allows repeated same-named entries,
  // e.g. `[LegacyFactoryFunction=A, LegacyFactoryFunction=B]`, index.bs:10806
  // — so a name-keyed map would silently lose duplicates), not a map keyed by
  // attribute name. So this compiles to `Cond.Any` — a real search over that
  // list for an entry whose `id` is the attribute name — rather than a
  // `HasField` nested-field-path shape (which would silently always evaluate
  // to `false`/`true`: a list checked for a string-keyed field just falls to
  // `Obj.exists`'s default `case _ => false`, no crash, just permanently
  // wrong). Of the 20 corpus occurrences of this idiom, 16 say "the" and 4
  // say "a" (no semantic difference — just which reads more naturally per
  // attribute); all 20 link "extended attribute" via `[=extended
  // attribute=]` — 3 in webidl/index.bs's `attribute setter` used to leave it
  // plain text (docs/spec_inconsistencies.md #19), normalized to the linked
  // form by `SpecPatch` #49 rather than tolerated here, unlike `HasSlotPos`/
  // `Neg`'s still-generous handling of the sibling "internal slot" markup gap
  // above (docs/spec_inconsistencies.md #18) — deliberately stricter, so a
  // spec edit that reintroduces an unlinked "extended attribute" here falls
  // through to `Unknown`/`EYet` instead of silently parsing anyway. The
  // subject is restricted to a bare `|var|` (every real
  // atomic occurrence is one — |interface|/|attribute|/|operation|), unlike
  // HasSlotPos/Neg's open `(.+?)`: this idiom also shows up nested inside a
  // relative clause ("|interface| is in the set of [=inherited interfaces=]
  // of an interface that is declared with the [{{Global}}] [=extended
  // attribute=]", webidl_yet_categorized.md category II-C) where "declared
  // with" grammatically describes the existentially-quantified *ancestor*
  // interface ("an interface that ..."), not whatever text happens to
  // precede it. An open `(.+?)` would swallow that whole relative clause as
  // if it were this subject, producing a nonsensical `Field` base; requiring
  // a bare pipe-var subject can only match the genuine atomic shape and
  // leaves the II-C composite sentence to fall through to `Unknown`, same as
  // before this pattern existed.
  private val DeclaredWithAttrPos =
    """(?si)^(\|[^|]+\|)\s+is\s+declared with\s+(?:the|an?)\s+\[\{\{([^}]+)\}\}\]\s+\[=extended attribute=\]$""".r
  private val DeclaredWithAttrNeg =
    """(?si)^(\|[^|]+\|)\s+is not\s+declared with\s+(?:the|an?)\s+\[\{\{([^}]+)\}\}\]\s+\[=extended attribute=\]$""".r
  // "X was [not] specified with the [{{Y}}] extended attribute" — a verb
  // synonym of DeclaredWithAttrPos/Neg above, seen only for {{LegacyLenientThis}}
  // (webidl_yet_categorized.md category II-A's `#6-8`/`#7-12`,
  // webidl/index.bs:12365/12416 — every other extended-attribute check in the
  // corpus uses "is [not] declared with"). Same subject/shape restrictions as
  // DeclaredWithAttrPos/Neg, for the same reasons; routes to the same
  // `declaredWithAttr` helper.
  private val SpecifiedWithAttrPos =
    """(?si)^(\|[^|]+\|)\s+was\s+specified with\s+(?:the|an?)\s+\[\{\{([^}]+)\}\}\]\s+\[=extended attribute=\]$""".r
  private val SpecifiedWithAttrNeg =
    """(?si)^(\|[^|]+\|)\s+was not\s+specified with\s+(?:the|an?)\s+\[\{\{([^}]+)\}\}\]\s+\[=extended attribute=\]$""".r
  // "the [{{Y}}] extended attribute was not specified on X" — DeclaredWithAttrNeg
  // with subject and attribute-name swapped (webidl_yet_categorized.md category
  // II-A's `#2-8`, webidl/index.bs:12093). Only this negative, reversed-order
  // phrasing appears in the corpus (no positive counterpart), so only that one
  // pattern is added, per this file's usual practice of matching just the
  // shapes actually seen rather than a hypothetical full set.
  private val AttrSpecifiedOnNeg =
    """(?si)^the\s+\[\{\{([^}]+)\}\}\]\s+\[=extended attribute=\]\s+was not specified on\s+(\|[^|]+\|)$""".r
  // "X has any [=member=] declared with the [{{Y}}] extended attribute" —
  // webidl_yet_categorized.md category II-A's `#2-6` (webidl/index.bs:12074).
  // An existential over `X.members` rather than a direct check on `X` itself —
  // `Any`'s own body reuses `declaredWithAttr` with a synthesized `|member|`
  // binder, the same "prepend a fresh binder as the elided subject" trick
  // `InInheritedInterfacesOfDeclared` below and `AnyIn` above already use. Only
  // the positive phrasing appears in the corpus.
  private val HasAnyMemberDeclaredWithAttr =
    """(?si)^(\|[^|]+\|)\s+has any\s+\[=member=\]\s+declared with\s+(?:the|an?)\s+\[\{\{([^}]+)\}\}\]\s+\[=extended attribute=\]$""".r
  // "X was [not] declared with a [=constructor operation=]" —
  // webidl_yet_categorized.md category II-A's `#3-2`/`#3-9`
  // (webidl/index.bs:11941/11974). Not an extended-attribute check at all —
  // `[=constructor operation=]` is a dfn link, not a `{{braced}}` IDL name —
  // the real question is whether any of `X.members` is a constructor
  // (`Definition.scala`'s `MemberKind.Constructor`; `Initialize.scala` seeds
  // every member record's `kind` field with exactly `MemberKind#toString`, per
  // `ExpandGetMemberPass`'s own doc). Same `Any`-over-`members` shape as
  // `HasAnyMemberDeclaredWithAttr` above, just searching by `kind` instead of
  // `extendedAttributes`.
  private val DeclaredWithConstructorOpPos =
    """(?si)^(\|[^|]+\|)\s+was\s+declared with\s+a\s+\[=constructor operation=\]$""".r
  private val DeclaredWithConstructorOpNeg =
    """(?si)^(\|[^|]+\|)\s+was not\s+declared with\s+a\s+\[=constructor operation=\]$""".r
  // "X is [=read only=] and does not have a [{{A}}], [{{B}}] or [{{C}}]
  // extended attribute" — webidl_yet_categorized.md category II-A's `#7-2`
  // (webidl/index.bs:12396). Matched as one whole compound shape *before* the
  // generic top-level " or "/" and " split in `parse` below, for the same
  // reason as `IsOneOfPos`/`Neg`/`LinkCallArgsEndsBool` above: the attribute
  // list's own internal " or " (between {{PutForwards}} and {{Replaceable}})
  // would otherwise be mistaken by that generic search for the sentence's
  // real, outer connective, cutting the sentence apart in the wrong place
  // (confirmed empirically — without this, `InstrParser.splitCondAndRest`'s own
  // top-level comma scan also mis-splits this same sentence's cond/action
  // boundary, fixed separately via `TextSplit.bracedListSpans`). The lazy
  // `(.+?)` subject group stops at the first " is [=read only=] and does not
  // have " — every real occurrence's subject is a bare `|attribute|`, but kept
  // general like `IsOneOfPos`/`Neg`'s `lhsRaw` rather than restricted to a
  // pipe-var, since nothing here requires that restriction.
  private val ReadOnlyAndLacksAnyOfAttrList =
    s"""(?si)^(.+?)\\s+is\\s+\\[=read only=\\]\\s+and\\s+does not have\\s+(?:a|an)\\s+($BracedItemList)\\s+\\[=extended attribute=\\]$$""".r
  private val BracedAttrName = """\[\{\{([^}]+)\}\}\]""".r
  // Interface capability predicates — webidl_yet_categorized.md category
  // II-D. Each is a search over `X.members` by `kind`, the same shape as
  // `DeclaredWithConstructorOpPos`/`Neg` above (see `hasMemberOfKind`):
  //   - "X [=support indexed properties|supports indexed properties=]" /
  //     "X has an [=indexed property getter=]" -> `IndexedGetter` (an
  //     interface supports indexed properties iff it defines an indexed
  //     property getter, webidl/index.bs:2757-2758)
  //   - "X [=support named properties|supports named properties=]" ->
  //     `NamedGetter` (likewise, index.bs:2930-2933)
  //   - "X has a [=pair iterator=]" -> `Iterable` *and* the member is a
  //     pair (`iterable<K, V>`), not a value iterator (`iterable<V>`,
  //     index.bs:3950-3955). `Iterable` alone doesn't tell the two apart and
  //     the member record has no type-argument field yet, so the pair check
  //     stays a `Cond.Unknown` (`yet`) inside the search's body.
  //   - "X does not have an [=asynchronously iterable declaration=] (of
  //     either sort)" -> not `AsyncIterable` (value and pair variants share
  //     one kind, so "of either sort" is just the plain membership test)
  // The link text may or may not carry a `|display text|` alias, so it's
  // optional in each pattern.
  private val SupportsIndexedProps =
    """(?si)^(\|[^|]+\|)\s+\[=support indexed properties(?:\|[^\]]*)?=\]$""".r
  private val SupportsNamedProps =
    """(?si)^(\|[^|]+\|)\s+\[=support named properties(?:\|[^\]]*)?=\]$""".r
  private val HasIndexedGetterPos =
    """(?si)^(\|[^|]+\|)\s+has an\s+\[=indexed property getter=\]$""".r
  private val HasPairIteratorPos =
    """(?si)^(\|[^|]+\|)\s+has a\s+\[=pair iterator=\]$""".r
  private val HasAsyncIterableNeg =
    """(?si)^(\|[^|]+\|)\s+does not have an\s+\[=asynchronously iterable declaration=\](?:\s+\(of either\s+sort\))?$""".r
  // "X contains an [=interface=] which [=support indexed properties|supports
  // indexed properties=], [=support named properties|named properties=], or
  // both" — `#1-6` (index.bs:13862-13864). An existential over the list `X`
  // whose body is itself a member-kind search, `IndexedGetter` or
  // `NamedGetter` ("or both" adds nothing to an inclusive or). Matched as one
  // whole shape before `parse`'s generic top-level " or " split, which would
  // otherwise cut at the list's own ", or" (`InstrParser.splitCondAndRest`
  // likewise protects it via `TextSplit.supportsPropsListSpans`).
  private val ContainsIfaceSupportingIndexedOrNamed =
    s"""(?si)^(\\|[^|]+\\|)\\s+contains an\\s+\\[=interface=\\]\\s+which\\s+($SupportsPropsList)$$""".r
  // "X is [not] declared to inherit from another interface" —
  // webidl_yet_categorized.md category II-C's `#2-2` (`webidl/index.bs:12055`).
  // `Initialize.scala` seeds every interface/namespace record's `"inherit"`
  // field as either the parent interface's own record (an `Addr`) or `Null`
  // when it doesn't inherit — always *present* as a key either way, so
  // `HasField`/`EExists` (which checks key presence, not value) would always
  // be `true` here regardless of whether there's a real parent; `Eq(...,
  // SpecTerm("null"))` is the correct test (`Compiler.compileExpr` already
  // lowers `SpecTerm("null")` to `ENull()`).
  private val DeclaredToInheritPos =
    """(?si)^(\|[^|]+\|)\s+is\s+declared to inherit from another interface$""".r
  private val DeclaredToInheritNeg =
    """(?si)^(\|[^|]+\|)\s+is not\s+declared to inherit from another interface$""".r
  // "X inherits from some other interface |P|" — webidl_yet_categorized.md
  // category II-C's `#3-8` (`webidl/index.bs:11962`). Unlike
  // DeclaredToInheritPos above, `P` is a named binder the *body* this
  // condition guards goes on to reference directly (e.g. "then set
  // |constructorProto| to the [=interface object=] of |P| in |realm|") — so
  // this reuses `Cond.Exists`'s "value-producing existential" shape (`P`
  // equals `X.inherit`) rather than a plain boolean, so a later lowering pass
  // (`ExpandExistentialsPass`) can turn it into both a real null-check *and* a
  // real `Let` binding `P` before the guarded body runs.
  private val InheritsFromOtherInterface =
    """(?si)^(\|[^|]+\|)\s+inherits from some other interface\s+\|(\w+)\|$""".r
  // "X is in the set of [=inherited interfaces=] of an interface that
  // CLAUSE" — webidl_yet_categorized.md category II-C (index.bs:12064-12066).
  // [=inherited interfaces=] of I is the set of interfaces I inherits from
  // (index.bs:695), so "X is in the set of inherited interfaces of Y" means X
  // is an ancestor of Y — the bound "an interface that CLAUSE" is the
  // *descendant* (it inherits from X), not the ancestor. Synthesize a
  // `|descendant|` binder, re-parse CLAUSE with it prepended (reuses
  // DeclaredWithAttrPos/Neg above), and reuse Contains for "in the set of X"
  // (same idiom as "is contained in X").
  //
  // [2026-09-14] Made real: `descendant` now ranges over `Initialize.scala`'s
  // `HOST_DEFINED.interfaces` registry (`Compiler`'s
  // `SpecTerm("all interfaces")` case) via `Cond.Any` instead of the
  // unhoistable `Cond.Exists` this used to produce, and the membership check
  // reuses `inclusive_inherited_interfaces` (already extracted/compiled,
  // index.bs:707-718 — `[=inherited interfaces=]` itself is still just prose,
  // never a real callable) called on `descendant.inherit` rather than
  // `descendant` itself: `inclusive_inherited_interfaces(descendant)` is
  // `[descendant, descendant.inherit, ...]`, so calling it one field over
  // gives exactly `[descendant.inherit, descendant.inherit.inherit, ...]` —
  // the true *exclusive* inherited-interfaces set index.bs:695-700 defines,
  // with no separate "exclude self" step needed. That algorithm's own loop
  // never assumes its argument is non-null (`while interface != null`), so a
  // `descendant` with no parent at all correctly short-circuits to `«»`.
  private val InInheritedInterfacesOfDeclared =
    """(?si)^(\|[^|]+\|)\s+is\s+in the set of\s+\[=inherited interfaces=\]\s+of an interface that\s+(.+)$""".r
  // "X contains any duplicates" / "X contains no duplicates" / "X does not
  // contain any duplicates" — see index.bs:1863.
  private val ContainsDuplicatesNeg =
    """(?si)^(.+?)\s+(?:contains no duplicates|does not contain any duplicates)$""".r
  private val ContainsDuplicatesPos =
    """(?si)^(.+?)\s+contains any duplicates$""".r
  // "[=algo|display=] for ARG1 [with ARG2[, ...] [and ARGN]] IS/RETURNS
  // BOOL/null" — a spec call phrased with "for"/"with"/"and" as its own
  // English argument-list connectors (mirrors the "from X, enabled Y, and Z"
  // phrasing an algorithm's own <dfn> head uses for its parameter list),
  // immediately compared against a boolean or null result. Matched as a whole
  // *before* the generic and/or top-level split in `parse` below, since that
  // split can't tell this "and" apart from a real boolean and — splitting
  // first severs the last argument from its call (see
  // index.bs:411,423,455,737,765, all "... for |module| with |builtinSetNames|
  // and |importedStringModule| returns/is false", plus index.bs:737's
  // "[=find a builtin=] for (|moduleName|, |name|, |type|) and
  // |builtinSetNames| is not null"). Each argument token is either a bare
  // `|var|` or a parenthesized untagged tuple `(|a|, |b|, ...)` (the only
  // other multi-arg shape this idiom's call sites actually use, per
  // `ExprParser.TuplePat`) — not free text — so this can't accidentally
  // swallow a genuine "COND1 and COND2" where COND1 itself happens to read
  // "... for X is Y".
  private val ArgToken = """(?:\([^()]*\)|\|[^|]+\|)"""
  private val LinkCallArgsEndsBool =
    s"""(?si)^(\\[=[^\\]]+\\])\\s+((?:for|with)\\s+$ArgToken(?:\\s*(?:,\\s*|and\\s+|with\\s+)$ArgToken)*)\\s+(is not|is|returns)\\s+(true|false|null)$$""".r

  // "EXPR is [not] one of A, B[, ...] or C" — an explicit disjunction of dfn-
  // linked terms (index.bs:521's "|valtype| is one of [=i32=], [=f32=] or
  // [=f64=]"), as opposed to WebIDL's structurally similar "|S| is not one of
  // |E|'s [=enumeration values=]" (webidl/index.bs:8068/12451, a *list
  // membership* check against a single collection expression, not an
  // enumerated disjunction of terms) — that shape hasn't been hit by any test
  // case yet; if it is, it needs a different Cond (a `Contains`-style check
  // over the enumeration's values), not this list-of-Eq expansion. Each item
  // is restricted to a `[=dfn link=]` (every real occurrence is one) so the
  // list's own internal " or "/", " separators can be matched precisely,
  // rather than swallowing a real top-level " or "/" and " that happens to
  // follow the list (same hazard, and same "match the whole shape before the
  // generic top-level split" fix, as `LinkCallArgsEndsBool` above — index.bs:
  // 521 itself is "|valtype| is one of [=i32=], [=f32=] or [=f64=] and |v|
  // [=is not a Number=]", where the list's own "or" would otherwise be
  // mistaken by `parse`'s top-level `findTopLevel(_, " or ")` for the
  // sentence's real, outer connective). An optional trailing `" and "`/
  // `" or "` + rest is captured and recursively parsed so this composes
  // correctly with whatever follows.
  private val IsOneOfNeg =
    s"""(?si)^(.+?)\\s+is not one of\\s+($EnumList)(?:\\s+(and|or)\\s+(.+))?$$""".r
  private val IsOneOfPos =
    s"""(?si)^(.+?)\\s+is one of\\s+($EnumList)(?:\\s+(and|or)\\s+(.+))?$$""".r

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
  // "If allocation fails, ..." — js-api's mem_alloc/table_alloc call sites
  // (index.bs:879/1051), a bare no-subject condition; see Cond.AllocationFails'
  // own doc for why this can't be resolved to a real `[=error=]` comparison
  // here (no access to the preceding step's bound variable at this level).
  private val AllocationFailsStep = """(?si)^allocation fails$""".r
  // "If this [operation] throws an exception, ..." (untyped) or
  // "If this [operation] throws a {{TypeError}}, ..." (typed): group 1 is the
  // exception type name when the typed form matched, else null.
  private val ThrowsError =
    """(?si)^this(?: operation)? throws (?:an? \{\{([^}]+)\}\})$""".r
  private val ThrowsException =
    """(?si)^this(?: operation)? throws an exception$""".r

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
        val rhs =
          if boolStr.equalsIgnoreCase("null") then SpecTerm("null")
          else Bool(boolStr.equalsIgnoreCase("true"))
        Eq(
          ExprParser.parse(s"$link $argsPhrase"),
          rhs,
          negated = isKind.trim.equalsIgnoreCase("is not"),
        )
      case IsOneOfNeg(lhsRaw, listRaw, connector, restRaw) =>
        composeConnector(buildIsOneOf(lhsRaw, listRaw, negated = true))(
          connector,
          restRaw,
        )
      case IsOneOfPos(lhsRaw, listRaw, connector, restRaw) =>
        composeConnector(buildIsOneOf(lhsRaw, listRaw, negated = false))(
          connector,
          restRaw,
        )
      case AnyIn(binder, collsRaw, predTail) =>
        val collections = collsRaw.split("""\s+or\s+""").toList.map { c =>
          ExprParser.parse(c)
        }
        Any(binder, collections, parse(s"|$binder| $predTail"))
      case ExistsSuchThat(binder, body) =>
        Exists(binder, parse(body))
      case ContainsIfaceSupportingIndexedOrNamed(exprRaw, _) =>
        Any(
          "iface",
          List(ExprParser.parse(exprRaw)),
          hasMemberOfKind(
            "|iface|",
            List(MemberKind.IndexedGetter, MemberKind.NamedGetter),
          ),
        )
      case ReadOnlyAndLacksAnyOfAttrList(exprRaw, listRaw) =>
        val readOnly =
          Eq(Field(ExprParser.parse(exprRaw), "readonly"), Bool(true))
        val attrNames =
          BracedAttrName.findAllMatchIn(listRaw).map(_.group(1)).toList
        val lacksAll = attrNames
          .map(name => declaredWithAttr(exprRaw, name, negated = true))
          .reduceLeft(And.apply)
        And(readOnly, lacksAll)
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
            // strip a trailing comma before "or" (e.g. "A, or B") the same
            // way the "and" branch below already does for "A, and B" — a
            // comma-before-connective is just list punctuation, not part of
            // the left condition's own text (see index.bs:12064's "|interface|
            // is declared with the [{{Global}}] [=extended attribute=], or
            // |interface| is in the set of ...").
            val left = s.substring(0, i).trim.stripSuffix(",").trim
            Or(parse(left), parseOrAbbreviated(s.substring(i + 4).trim))
          case None =>
            findTopLevel(searchIn, " and ") match
              // "X ..., and therefore is a Y" (index.bs:508's only
              // occurrence) -- "and therefore" isn't a genuine second
              // condition, it's a rhetorical restatement of a consequence
              // already implied by the clause it follows ("|v| has a
              // [[FunctionAddress]] internal slot, and therefore is an
              // [=Exported Function=]" -- being an Exported Function *is*
              // having that slot, per `ExpandExportedObjectIsTypePass`'s own
              // "Exported Function" -> `HasSlot(_, "FunctionAddress")"
              // lowering, so parsing it out separately would just `&&` the
              // same check against itself). Dropping the clause entirely
              // keeps the same meaning as the left clause alone, without
              // needing to reconstruct "therefore"'s elided subject.
              case Some(i) if s.substring(i + 5).trim.startsWith("therefore") =>
                parse(s.substring(0, i).trim.stripSuffix(",").trim)
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

  /** "X is [not] one of A, B[, ...] or C" — desugars to a chain of per-item
    * `Eq`s: positive is an `Or` ("X equals one of them"), negative is an `And`
    * of negated `Eq`s (De Morgan's — "X equals none of them"), since [[Cond]]
    * has no standalone negation node.
    */
  private def buildIsOneOf(
    lhsRaw: String,
    listRaw: String,
    negated: Boolean,
  ): Cond =
    val lhs = ExprParser.parse(lhsRaw)
    val items = listRaw
      .split(",")
      .toList
      .flatMap(_.split("""\s+or\s+"""))
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(item => Eq(lhs, ExprParser.parse(item), negated))
    items.reduceLeft(if negated then And.apply else Or.apply)

  /** Recombines `base` with whatever `" and "`/`" or "` + rest followed it in
    * the original text (both [[IsOneOfNeg]]/[[IsOneOfPos]] capture this as an
    * optional trailing group, `connector`/`restRaw` null when absent) — reuses
    * `parseOrAbbreviated` for `restRaw` so it composes with the same fallback
    * every other top-level and/or split gets.
    */
  private def composeConnector(base: Cond)(
    connector: String,
    restRaw: String,
  ): Cond =
    Option(connector) match
      case Some("and") => And(base, parseOrAbbreviated(restRaw))
      case Some("or")  => Or(base, parseOrAbbreviated(restRaw))
      case _           => base

  /** "X is [not] declared with the [{{ATTR}}] extended attribute" — a search
    * over `X.extendedAttributes` (a list, see `DeclaredWithAttrPos`'s own doc)
    * for an entry whose `id` is `attrName`.
    */
  private def declaredWithAttr(
    exprRaw: String,
    attrName: String,
    negated: Boolean = false,
  ): Cond =
    Any(
      "ea",
      List(Field(ExprParser.parse(exprRaw), "extendedAttributes")),
      Eq(Field(Var("ea"), "id"), Str(attrName)),
      negated,
    )

  /** "X was [not] declared with a [=constructor operation=]" — a search over
    * `X.members` (see `DeclaredWithConstructorOpPos`'s own doc) for one whose
    * `kind` is the `Constructor` enum.
    */
  private def declaredWithConstructorOp(
    exprRaw: String,
    negated: Boolean = false,
  ): Cond = hasMemberOfKind(exprRaw, List(MemberKind.Constructor), negated)

  /** a search over `X.members` for one whose `kind` is any of `kinds`
    * (`Initialize.scala` seeds `kind` as `Enum(MemberKind#toString)`, so a
    * `Str` comparison would never match).
    */
  private def hasMemberOfKind(
    exprRaw: String,
    kinds: List[MemberKind],
    negated: Boolean = false,
  ): Cond =
    Any(
      "m",
      List(Field(ExprParser.parse(exprRaw), "members")),
      kinds
        .map(k => Eq(Field(Var("m"), "kind"), Enum(k.toString)))
        .reduceLeft(Or.apply),
      negated,
    )

  private def parseAtomic(s: String): Cond = s match
    case UnreachableStep()     => Unreachable
    case ThrowsError(kind)     => Throws(Some(kind))
    case ThrowsException()     => Throws(None, Option("|exception|"))
    case AllocationFailsStep() => AllocationFails
    case MapExistsPos(baseRaw) => HasField(ExprParser.parse(baseRaw))
    case MapExistsNeg(baseRaw) =>
      HasField(ExprParser.parse(baseRaw), negated = true)
    case HasBeenInitializedPos(baseRaw) => HasField(ExprParser.parse(baseRaw))
    case HasBeenInitializedNeg(baseRaw) =>
      HasField(ExprParser.parse(baseRaw), negated = true)
    case ListIsEmpty(baseRaw, alias) =>
      val negated = Option(alias).exists(_.toLowerCase.contains("not"))
      Eq(Length(ExprParser.parse(baseRaw)), Num("0"), negated)
    case ImplementsPos(exprRaw, faceRaw) =>
      Implements(ExprParser.parse(exprRaw), ExprParser.parse(faceRaw))
    case ImplementsNeg(exprRaw, faceRaw) =>
      Implements(
        ExprParser.parse(exprRaw),
        ExprParser.parse(faceRaw),
        negated = true,
      )
    case HasSlotPos(exprRaw, slot) =>
      HasSlot(ExprParser.parse(exprRaw), slot)
    case HasSlotNeg(exprRaw, slot) =>
      HasSlot(ExprParser.parse(exprRaw), slot, negated = true)
    case ContainsDuplicatesNeg(exprRaw) =>
      HasDuplicates(ExprParser.parse(exprRaw), negated = true)
    case ContainsDuplicatesPos(exprRaw) =>
      HasDuplicates(ExprParser.parse(exprRaw))
    case DeclaredWithAttrPos(exprRaw, attrName) =>
      declaredWithAttr(exprRaw, attrName)
    case DeclaredWithAttrNeg(exprRaw, attrName) =>
      declaredWithAttr(exprRaw, attrName, negated = true)
    case SpecifiedWithAttrPos(exprRaw, attrName) =>
      declaredWithAttr(exprRaw, attrName)
    case SpecifiedWithAttrNeg(exprRaw, attrName) =>
      declaredWithAttr(exprRaw, attrName, negated = true)
    case AttrSpecifiedOnNeg(attrName, exprRaw) =>
      declaredWithAttr(exprRaw, attrName, negated = true)
    case HasAnyMemberDeclaredWithAttr(exprRaw, attrName) =>
      Any(
        "member",
        List(Field(ExprParser.parse(exprRaw), "members")),
        declaredWithAttr("|member|", attrName),
      )
    case DeclaredWithConstructorOpPos(exprRaw) =>
      declaredWithConstructorOp(exprRaw)
    case DeclaredWithConstructorOpNeg(exprRaw) =>
      declaredWithConstructorOp(exprRaw, negated = true)
    case SupportsIndexedProps(exprRaw) =>
      hasMemberOfKind(exprRaw, List(MemberKind.IndexedGetter))
    case HasIndexedGetterPos(exprRaw) =>
      hasMemberOfKind(exprRaw, List(MemberKind.IndexedGetter))
    case SupportsNamedProps(exprRaw) =>
      hasMemberOfKind(exprRaw, List(MemberKind.NamedGetter))
    case HasPairIteratorPos(exprRaw) =>
      Any(
        "m",
        List(Field(ExprParser.parse(exprRaw), "members")),
        And(
          Eq(Field(Var("m"), "kind"), Enum(MemberKind.Iterable.toString)),
          Cond.Unknown("|m|'s type is pair"),
        ),
      )
    case HasAsyncIterableNeg(exprRaw) =>
      hasMemberOfKind(exprRaw, List(MemberKind.AsyncIterable), negated = true)
    case DeclaredToInheritPos(exprRaw) =>
      Eq(
        Field(ExprParser.parse(exprRaw), "inherit"),
        SpecTerm("null"),
        negated = true,
      )
    case DeclaredToInheritNeg(exprRaw) =>
      Eq(Field(ExprParser.parse(exprRaw), "inherit"), SpecTerm("null"))
    case InheritsFromOtherInterface(exprRaw, binder) =>
      Exists(
        binder,
        Eq(Field(ExprParser.parse(exprRaw), "inherit"), Var(binder)),
      )
    case InInheritedInterfacesOfDeclared(exprRaw, clauseRaw) =>
      val binder = "descendant"
      Any(
        binder,
        List(SpecTerm("all interfaces")),
        And(
          Contains(
            ExprParser.parse(exprRaw),
            AlgoCall(
              "[=inclusive inherited interfaces=]",
              List(Field(Var(binder), "inherit")),
            ),
          ),
          parse(s"|$binder| ${clauseRaw.trim}"),
        ),
      )
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
        // "X is [not] [=read only=]" — webidl_yet_categorized.md category
        // II-A's `#7-2` (webidl/index.bs:12394/12396) reads this off
        // `Attribute`'s own `readonly: Boolean` field (`Definition.scala`)
        // rather than falling through to the generic `Eq` case below, which
        // would otherwise compare `lhsRaw` against the bare term "read only"
        // — an accidental non-crashing parse (`(= attribute ~read only~)`),
        // never a real field read.
        case "[=read only=]" =>
          Eq(Field(ExprParser.parse(lhsRaw), "readonly"), Bool(true), negated)
        case ArticleLink(noun) =>
          IsType(ExprParser.parse(lhsRaw), noun, negated)
        case IsTheBracedInterfaceLink(name) =>
          IsType(ExprParser.parse(lhsRaw), name, negated)
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
