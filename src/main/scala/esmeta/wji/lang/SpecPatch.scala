package esmeta.wji.lang

/** Known corrections applied to `spectec/document/js-api/index.bs` and, for the
  * small subset of algorithms `SpecFile.webidlFilter` pulls in from it,
  * `webidl/index.bs`.
  *
  * Each entry is a `(from, to)` pair applied as a literal string replacement on
  * the raw source text before parsing, via [[apply]] (run over every file
  * `AlgorithmExtractor.extractFromFile` loads). Entries are grouped into
  * comment-delimited clusters by *what change they make* (e.g. "pass the
  * current realm", "make a type parameter explicit"), not by where they sit in
  * the source file, so that one pair makes exactly one kind of change. A site
  * that needs two kinds of fix (say, both a realm and a type parameter) gets
  * two separate pairs, one per cluster. Because [[apply]] folds pairs in list
  * order via literal `String.replace`, a pair anchored on text that only exists
  * after an earlier pair has run (e.g. a type-parameter pair anchored on a line
  * that a realm pair also touches) is always placed after it — noted inline
  * wherever it isn't obvious.
  *
  * Every cluster is also tagged with what kind of change it makes:
  *
  *   - *(spec bug)* fixes an actual defect in the spec prose itself — a typo,
  *     stale phrasing, or markup that violates the surrounding file's own
  *     conventions. These correspond 1-to-1 with the errors documented in
  *     `mechanize/spec_errors.md`.
  *   - *(suggestion)* makes an already-valid elision explicit — a spot where
  *     Bikeshed prose conventionally omits an argument (a generic type
  *     parameter, an implicit realm) that this project's extractor/compiler
  *     needs spelled out in order to compile. Nothing here is wrong in the spec
  *     as written; it's just not explicit enough for this tool.
  */
object SpecPatch:

  /** Renders webidl/index.bs's `of type <code>Promise&lt;X&gt;</code>` clause
    * for instantiating a generic Promise-returning operation's type parameter
    * at a call site (see `AlgorithmExtractor.GenericVarIgnore` /
    * `ExprParser.OfTypeGeneric`). Used throughout the patches below to annotate
    * `a new promise`/`resolve`/`react`/`reject` call sites with the type they
    * actually act on.
    */
  private def ofTypePromise(x: String): String =
    s"""of type <code><a interface>Promise</a>&lt;<a lt="interface type">$x</a>></code>"""

  /** All patches in application order. */
  val patches: List[(String, String)] = List(
    // #1 (spec bug) — empty ordered map literal written as « » instead of «[ ]»
    "Let |builtinOrStringImports| be the ordered map « »."
    -> "Let |builtinOrStringImports| be the ordered map «[ ]».",

    // #2 (spec bug) — variable `keys` missing pipe delimiters in [[OwnPropertyKeys]]
    "1. Let keys be a new empty list.\n    1. Return keys."
    -> "1. Let |keys| be a new empty list.\n    1. Return |keys|.",

    // #3 (TODO: this is temporary, and should be removed)
    // esmeta doesn't support overloaded methods, but {{WebAssembly}}'s
    // `instantiate` is overloaded (module-first and buffer-source-first
    // variants). Rename this one to avoid the collision.
    "The <dfn method for=\"WebAssembly\">instantiate(|moduleObject|, |importObject|)</dfn> method, when invoked, performs the following steps:"
    ->
    "The <dfn method for=\"WebAssembly\">instantiate_object(|moduleObject|, |importObject|)</dfn> method, when invoked, performs the following steps:",

    // #4 (spec bug) — pass the current realm explicitly wherever a callee
    // needs it but js-api/index.bs's caller elides it, the way Bikeshed prose
    // conventionally does. Two callees need it:
    //
    //   - `a new promise` (webidl/index.bs) already declares an explicit
    //     |realm| parameter, but all 3 call sites below invoke it with no
    //     argument. Unlike the type parameter each site also needs (#11
    //     below, which differs per site and so needs per-site anchoring),
    //     the realm fix is identical everywhere, so one pair matching just
    //     the shared opening line covers all 3.
    "1. Let |promise| be [=a new promise=]."
    ->
    "1. Let |promise| be [=a new promise=] in the [=current Realm=].",
    //   - `react` (webidl/index.bs) has no |realm| parameter at all: it tries
    //     to derive the realm for its returned promise capability by chasing
    //     |promise|.[[Promise]].[[Realm]] off the promise being reacted to,
    //     but {{Promise}} instances are ordinary objects, and ordinary
    //     objects carry no [[Realm]] internal slot in ECMA-262 (only function
    //     objects do) — there's no such slot to chase. Give `react` the same
    //     explicit |realm| parameter `a new promise` already has, source the
    //     {{Promise}} constructor from it directly, and supply it at both
    //     call sites in js-api/index.bs.
    "to a <code><a interface>Promise</a>&lt;|T|&gt;</code> |promise|, given one or two sets of steps"
    ->
    "to a <code><a interface>Promise</a>&lt;|T|&gt;</code> |promise| in a [=realm=] |realm|, given one or two sets of steps",
    "1.  Let |constructor| be |promise|.\\[[Promise]].\\[[Realm]].\\[[Intrinsics]].\\[[{{%Promise%}}]]."
    ->
    "1.  Let |constructor| be |realm|.\\[[Intrinsics]].\\[[{{%Promise%}}]].",
    "1. [=React=] to |promiseOfModule|:"
    ->
    "1. [=React=] to |promiseOfModule| in the [=current Realm=]:",
    "1. [=React=] to |innerPromise|:"
    ->
    "1. [=React=] to |innerPromise| in the [=current Realm=]:",

    // #5 (spec bug) — `|module|.[=imports=]` treats a decoded `module` as a
    // record with named fields, a leftover from an older version of the Wasm
    // Core Spec. In the current (post-GC-proposal) spec/SpecTec
    // representation, `module` is an opaque positional value with no named
    // fields — the only correct way to read its imports is through the
    // `module_imports` embedding function, exactly as every other use site in
    // this file already does (e.g. lines 405, 474, 497, 736).
    "1. If |module|.[=imports=] [=list/is empty|is not empty=], and |importObject| is undefined, throw a {{TypeError}} exception."
    ->
    "1. If [=module_imports=](|module|) [=list/is empty|is not empty=], and |importObject| is undefined, throw a {{TypeError}} exception.",

    // #6 (spec bug) — {{Global}}'s `value` getter and setter are both written
    // inside a single `<div algorithm>`, the only place in the file where two
    // algorithms share one block (`AlgorithmExtractor` extracts one
    // `Algorithm` per `<div algorithm>`, so this compiled into one function
    // whose body was the getter's `Return` followed — as unreachable text,
    // not unreachable code — by every one of the setter's own steps). Split
    // into two divs, disambiguated via `algorithm="..."` ids (matching
    // `read-the-imports`'s precedent) rather than a second `<dfn attribute
    // for="Global">value</dfn>`: Bikeshed dfns must be unique by (type, for,
    // linking text), and WebIDL's own spec text (e.g. `DOMException`'s
    // `name`/`message`/`code` in webidl/index.bs) confirms the convention is
    // exactly one dfn per attribute, with the setter referring to it only in
    // prose. Two small edits do this — tag the getter's own `<div>`, then
    // close it and open a new one right before the setter's prose — rather
    // than one from/to pair spanning (and duplicating) the whole unchanged
    // setter body in between.
    "<div algorithm>\n    The getter of the <dfn attribute for=\"Global\">value</dfn>"
    ->
    "<div algorithm=\"global-value-getter\">\n    The getter of the <dfn attribute for=\"Global\">value</dfn>",
    "\n\n    The setter of the value attribute of {{Global}}"
    ->
    "\n</div>\n\n<div algorithm=\"global-value-setter\">\n    The setter of the value attribute of {{Global}}",

    // #7 (spec bug) — the enum values passed to `[$GetValueFromBuffer$]` are
    // written as bare `Uint8`/`Unordered` instead of the
    // `{{uint8}}`/`{{unordered}}` WebIDL enum-reference syntax every other
    // call site in the file uses.
    """1.  For |i| in [=the range=] |offset| to |offset| + |length| &minus; 1, inclusive, set
        |bytes|[|i| &minus; |offset|] to [$GetValueFromBuffer$](|jsArrayBuffer|, |i|, Uint8,
        true, Unordered)."""
    ->
    "1.  For |i| in [=the range=] |offset| to |offset| + |length| &minus; 1, inclusive, set |bytes|[|i| &minus; |offset|] to [$GetValueFromBuffer$](|jsArrayBuffer|, |i|, {{uint8}}, true, {{unordered}}).",

    // #8 (spec bug) — `react`'s own definition ends its
    // `onFulfilled`/`onRejected` `CreateBuiltinFunction` steps with a colon
    // rather than a period, as if a nested list were about to follow. None
    // does; it's a plain typo.
    "1.  Let |onFulfilled| be [$CreateBuiltinFunction$](|onFulfilledSteps|, 1, \"\", « »):"
    ->
    "1.  Let |onFulfilled| be [$CreateBuiltinFunction$](|onFulfilledSteps|, 1, \"\", « »).",
    "1.  Let |onRejected| be [$CreateBuiltinFunction$](|onRejectedSteps|, 1, \"\", « »):"
    ->
    "1.  Let |onRejected| be [$CreateBuiltinFunction$](|onRejectedSteps|, 1, \"\", « »).",

    // #9 (spec bug) — `|builtinOrStringImports| [=map/exist|contains=]
    // |moduleName|` is the only "X contains Y" phrasing for a map-membership
    // check in the whole file; every other one (15+ occurrences, e.g. lines
    // 864, 1028, 1164, 1263) writes `|map|[|key|] [=map/exists=]` instead — a
    // leftover of `builtinOrStringImports` once being a list (see #1: its
    // empty-literal is still written `« »`, the *list* notation, not
    // `«[ ]»`) where "contains" made sense; the link text was swapped from
    // `[=list/contains=]` to `[=map/exist|contains=]` when the type changed,
    // but the surrounding sentence structure never followed. Rewritten to the
    // file's own established map-check idiom.
    "1. If |builtinOrStringImports| [=map/exist|contains=] |moduleName|,"
    -> "1. If |builtinOrStringImports|[|moduleName|] [=map/exists=],",

    // #10 (spec bug) — the compile algorithm's two CompileError rejections
    // are written as plain "reject ... exception" prose with no [=...=] link,
    // so they don't parse as a call to `reject` at all today (now parseable
    // thanks to ExprParser.NewExceptionExpr's "a {{X}} exception" rule and,
    // for the first site, InstrParser's bare "and return" rule). Bracket them
    // into real links; the type parameter each now needs is added separately
    // below (#11), anchored on this pair's `to` text.
    "1. If |module| is [=error=], reject |promise| with a {{CompileError}} exception and return."
    ->
    "1. If |module| is [=error=], [=reject=] |promise| with a {{CompileError}} exception and return.",
    "1. If [=validate builtins and imported string for a WebAssembly module|validating builtins and imported strings=] for |module| with |builtinSetNames| and |importedStringModule| is false, reject |promise| with a {{CompileError}} exception."
    ->
    "1. If [=validate builtins and imported string for a WebAssembly module|validating builtins and imported strings=] for |module| with |builtinSetNames| and |importedStringModule| is false, [=reject=] |promise| with a {{CompileError}} exception.",

    // #11 (suggestion) — `a new promise`, `resolve`, `react`, and `reject`
    // each declare a leading type parameter T (see
    // AlgorithmExtractor.GenericVarIgnore), but every real call site in this
    // file elides it, the same Bikeshed-conventional omission #4 fixed for
    // realm arguments. Supply it explicitly at each site, with the declared
    // type of the |promise|/|p| argument being acted on (traced from each
    // function's own `[=a new promise=] of type ...` declaration, or — for
    // [=React=] — the type of the promise being reacted to).
    //
    //   - `a new promise`'s 3 call sites. Anchored on the post-#4 text (the
    //     realm cluster already ran), so the type lands right after `a new
    //     promise` and before `in the [=current Realm=]`. The third site's
    //     anchor also includes its second line in its post-#4 (realm-patched)
    //     form, since #4's react/|promiseOfModule| pair already touched it.
    "1. Let |promise| be [=a new promise=] in the [=current Realm=].\n    1. Run the following steps [=in parallel=]:"
    ->
    s"1. Let |promise| be [=a new promise=] ${ofTypePromise("Module")} in the [=current Realm=].\n    1. Run the following steps [=in parallel=]:",
    "1. Let |promise| be [=a new promise=] in the [=current Realm=].\n    1. Let |module| be |moduleObject|.\\[[Module]]."
    ->
    s"1. Let |promise| be [=a new promise=] ${ofTypePromise("Instance")} in the [=current Realm=].\n    1. Let |module| be |moduleObject|.\\[[Module]].",
    "1. Let |promise| be [=a new promise=] in the [=current Realm=].\n    1. [=React=] to |promiseOfModule| in the [=current Realm=]:"
    ->
    s"1. Let |promise| be [=a new promise=] ${ofTypePromise("WebAssemblyInstantiatedSource")} in the [=current Realm=].\n    1. [=React=] to |promiseOfModule| in the [=current Realm=]:",
    //   - `resolve`/`reject` call sites.
    "1. [=Resolve=] |promise| with |moduleObject|."
    ->
    s"1. [=Resolve=] ${ofTypePromise("Module")} |promise| with |moduleObject|.",
    "If this operation throws an exception, catch it, [=reject=] |promise| with the exception, and return |promise|."
    ->
    s"If this operation throws an exception, catch it, [=reject=] ${ofTypePromise("Instance")} |promise| with the exception, and return |promise|.",
    // the "Instantiate the core..."/"initialize an instance object..." catch
    // clauses share byte-identical reject text, so each pair below is
    // anchored with its own preceding line to target just the one site
    // (mirroring the technique above for `a new promise`'s identical call
    // sites).
    "1.  [=Instantiate the core of a WebAssembly module=] |module| with |imports|, and let |instance| be the result.\n                If this throws an exception, catch it, [=reject=] |promise| with the exception, and terminate these substeps."
    ->
    s"1.  [=Instantiate the core of a WebAssembly module=] |module| with |imports|, and let |instance| be the result.\n                If this throws an exception, catch it, [=reject=] ${ofTypePromise("Instance")} |promise| with the exception, and terminate these substeps.",
    "1.  [=initialize an instance object|Initialize=] |instanceObject| from |module| and |instance|.\n                If this throws an exception, catch it, [=reject=] |promise| with the exception, and terminate these substeps."
    ->
    s"1.  [=initialize an instance object|Initialize=] |instanceObject| from |module| and |instance|.\n                If this throws an exception, catch it, [=reject=] ${ofTypePromise("Instance")} |promise| with the exception, and terminate these substeps.",
    "1. [=Resolve=] |promise| with |instanceObject|."
    ->
    s"1. [=Resolve=] ${ofTypePromise("Instance")} |promise| with |instanceObject|.",
    "1. [=Resolve=] |promise| with |result|."
    ->
    s"1. [=Resolve=] ${ofTypePromise("WebAssemblyInstantiatedSource")} |promise| with |result|.",
    // the innerPromise-rejected/promiseOfModule-rejected branches share
    // byte-identical reject text, anchored the same way as the reject pair
    // above.
    "* If |innerPromise| was rejected with reason |reason|:\n                    1. [=Reject=] |promise| with |reason|."
    ->
    s"* If |innerPromise| was rejected with reason |reason|:\n                    1. [=Reject=] ${ofTypePromise("WebAssemblyInstantiatedSource")} |promise| with |reason|.",
    "* If |promiseOfModule| was rejected with reason |reason|:\n            1. [=Reject=] |promise| with |reason|."
    ->
    s"* If |promiseOfModule| was rejected with reason |reason|:\n            1. [=Reject=] ${ofTypePromise("WebAssemblyInstantiatedSource")} |promise| with |reason|.",
    //   - the two CompileError `reject` sites bracketed into real links by
    //     #10 above; anchored on #10's `to` text.
    "1. If |module| is [=error=], [=reject=] |promise| with a {{CompileError}} exception and return."
    ->
    s"1. If |module| is [=error=], [=reject=] ${ofTypePromise("Module")} |promise| with a {{CompileError}} exception and return.",
    "1. If [=validate builtins and imported string for a WebAssembly module|validating builtins and imported strings=] for |module| with |builtinSetNames| and |importedStringModule| is false, [=reject=] |promise| with a {{CompileError}} exception."
    ->
    s"1. If [=validate builtins and imported string for a WebAssembly module|validating builtins and imported strings=] for |module| with |builtinSetNames| and |importedStringModule| is false, [=reject=] ${ofTypePromise("Module")} |promise| with a {{CompileError}} exception.",
    //   - `react`'s 2 call sites. Anchored on the post-#4 text (realm cluster
    //     already ran), so the type lands right after [=React=] and before
    //     `to |...|`.
    "1. [=React=] to |promiseOfModule| in the [=current Realm=]:"
    ->
    s"1. [=React=] ${ofTypePromise("Module")} to |promiseOfModule| in the [=current Realm=]:",
    "1. [=React=] to |innerPromise| in the [=current Realm=]:"
    ->
    s"1. [=React=] ${ofTypePromise("Instance")} to |innerPromise| in the [=current Realm=]:",
  )

  def apply(source: String): String =
    patches.foldLeft(source) { case (s, (from, to)) => s.replace(from, to) }
