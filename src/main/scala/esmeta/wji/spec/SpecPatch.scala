package esmeta.wji.spec

/** Known corrections applied to `spectec/document/js-api/index.bs` and, for the
  * small subset of algorithms `SpecFile.webidlFilter` pulls in from it,
  * `webidl/index.bs`.
  *
  * Each entry is a `(from, to)` pair applied as a literal string replacement on
  * the raw source text before parsing, via [[apply]] (run over every file
  * `AlgorithmExtractor.extractFromFile` loads). Every numbered comment below is
  * tagged with what kind of change it makes — a single entry can mix both
  * within one line, so the tag lives in the comment rather than in separate
  * lists:
  *
  *   - *(spec bug)* fixes an actual defect in the spec prose itself — a typo,
  *     stale phrasing, or markup that violates the surrounding file's own
  *     conventions.
  *   - *(suggestion)* makes an already-valid elision explicit — a spot where
  *     Bikeshed prose conventionally omits an argument (e.g. a generic type
  *     parameter) that this project's extractor/compiler needs spelled out in
  *     order to compile. Nothing here is wrong in the spec as written; it's
  *     just not explicit enough for this tool.
  *   - *(hardcoding)* invents a call convention with no literal counterpart in
  *     the spec prose at all — needed where this project can't yet parse a
  *     shape the spec actually uses, rather than just an elided argument.
  *     Currently only `react`'s call sites (see #12): the spec nests a call's
  *     fulfilled/rejected continuations as bulleted branches directly under
  *     `[=React=] to |X|:`, a shape nothing in this project parses specially,
  *     so they're rewritten as two named closures passed explicitly instead.
  *   - *(spec inconsistency)* nothing here is independently wrong — the
  *     phrasing/markup is valid on its own — but it deviates from a pattern the
  *     *same* document already establishes at other, structurally identical
  *     call sites, in a way that breaks mechanization (e.g. one variant of a
  *     repeated idiom omitting a link/qualifier the other variants all
  *     include). Distinct from *(spec bug)*, which is wrong on its own terms
  *     without needing a sibling to compare against.
  *
  * REQUIRED, every time you add a *(spec bug)* or *(spec inconsistency)* entry:
  * add a matching numbered section to `docs/spec_errors.md` (for a spec bug) or
  * `docs/spec_inconsistencies.md` (for a spec inconsistency) in the SAME change
  * (title, File/Current/Expected/Reason, same shape as each doc's existing
  * entries). Those two files are what actually get reported upstream to the
  * spec authors — a patch added here without a matching entry there never
  * reaches them and just silently rots as tribal knowledge in this file. Do not
  * defer this to a follow-up commit; do it now, in the same patch that adds the
  * entry below.
  *
  * The *(suggestion)* type-parameter fix shows up folded into several numbered
  * entries below rather than as a pattern of its own, so it's called out here
  * instead of repeating it at every site: `a new promise`, `resolve`, `react`,
  * and `reject` (webidl/index.bs) each declare a leading type parameter T (see
  * `AlgorithmExtractor.GenericVarIgnore`), but every real call site elides it.
  * Fixed in #4, #10, #11, and #12, each supplying the type of the `promise`/`p`
  * argument being acted on at that site.
  *
  * (Not the same pattern as the *calling realm* fix also folded into #4 and #12
  * — that one is a genuine *(spec bug)*, not a suggestion: `a new
  * promise`/`react` declare a *required* `|realm|` parameter with no default,
  * and every js-api/index.bs call site omits it outright, leaving nothing to
  * bind it to. See docs/spec_errors.md #3 and #4/#12's own comments below.)
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
    "the ordered map « »" -> "the ordered map «[ ]»",

    // #2 (spec bug) — variable `keys` missing pipe delimiters in [[OwnPropertyKeys]]
    "Let keys be" -> "Let |keys| be",
    "Return keys." -> "Return |keys|.",

    // #3 (hardcoding) — esmeta doesn't support overloaded methods, but
    // {{WebAssembly}}'s `instantiate` is overloaded (module-first and
    // buffer-source-first variants). Renames the module-first one to avoid
    // the collision — a naming distinction the spec itself doesn't make,
    // invented purely to route around this project's lack of overload
    // support.
    "The <dfn method for=\"WebAssembly\">instantiate(|moduleObject|, |importObject|)</dfn> method, when invoked, performs the following steps:"
    ->
    "The <dfn method for=\"WebAssembly\">instantiate_object(|moduleObject|, |importObject|)</dfn> method, when invoked, performs the following steps:",
    "Promise&lt;Instance> instantiate("
    ->
    "Promise&lt;Instance> instantiate_object(",

    // #4 (spec bug, docs/spec_errors.md #3; and suggestion) — two distinct
    // fixes bundled into the same replacements below, since both land on the
    // same "1. Let |promise| be [=a new promise=]." call sites:
    //   - (spec bug, docs/spec_errors.md #3) `a new promise` is called with
    //     no |realm| argument, though its own declaration requires one.
    //   - (suggestion) `a new promise` also declares a leading type
    //     parameter T, elided at every call site the way a generic type
    //     argument conventionally is — nothing wrong in the spec as written,
    //     just not explicit enough for this tool.
    // All 3 call sites open with the same "1. Let |promise| be [=a new
    // promise=]." line, so each patch below is anchored with enough of the
    // following line to target just the one site (and is given the type the
    // promise is actually resolved with at that site: Module, Instance, and
    // WebAssemblyInstantiatedSource respectively).
    "1. Let |promise| be [=a new promise=].\n    1. Run the following steps [=in parallel=]:"
    ->
    s"1. Let |promise| be [=a new promise=] ${ofTypePromise("Module")} in the [=current Realm=].\n    1. Run the following steps [=in parallel=]:",
    "1. Let |promise| be [=a new promise=].\n    1. Let |module| be |moduleObject|.\\[[Module]]."
    ->
    s"1. Let |promise| be [=a new promise=] ${ofTypePromise("Instance")} in the [=current Realm=].\n    1. Let |module| be |moduleObject|.\\[[Module]].",

    // #5 (spec bug) — `|module|.[=imports=]` treats a decoded `module` as a
    // record with named fields, a leftover from an older version of the Wasm
    // Core Spec. In the current (post-GC-proposal) spec/SpecTec
    // representation, `module` is an opaque positional value with no named
    // fields — the only correct way to read its imports is through the
    // `module_imports` embedding function, exactly as every other use site in
    // this file already does (e.g. lines 405, 474, 497, 736).
    "|module|.[=imports=]" -> "[=module_imports=](|module|)",

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

    // #10 (suggestion) — `resolve`/`react`/`reject` each declare a leading
    // type parameter T (see AlgorithmExtractor.GenericVarIgnore), but every
    // real call site in this file elides it, the same gap #4 above fixed for
    // `a new promise`. Supply it explicitly at each site, with the declared
    // type of the |promise|/|p| argument being acted on (traced from each
    // function's own `[=a new promise=] of type ...` declaration, or — for
    // [=React=] — the type of the promise being reacted to).
    "1. [=Resolve=] |promise| with |moduleObject|."
    ->
    s"1. [=Resolve=] ${ofTypePromise("Module")} |promise| with |moduleObject|.",
    "If this operation throws an exception, catch it, [=reject=] |promise| with the exception, and return |promise|."
    ->
    s"If this operation throws an exception, catch it, [=reject=] ${ofTypePromise("Instance")} |promise| with the exception, and return |promise|.",
    // the "Instantiate the core..."/"initialize an instance object..." catch
    // clauses share byte-identical reject text, so each patch below is
    // anchored with its own preceding line to target just the one site
    // (mirroring #4's technique for `a new promise`'s identical call sites).
    "1.  [=Instantiate the core of a WebAssembly module=] |module| with |imports|, and let |instance| be the result.\n                If this throws an exception, catch it, [=reject=] |promise| with the exception, and terminate these substeps."
    ->
    s"1.  [=Instantiate the core of a WebAssembly module=] |module| with |imports|, and let |instance| be the result.\n                If this throws an exception, catch it, [=reject=] ${ofTypePromise("Instance")} |promise| with the exception, and terminate these substeps.",
    "1.  [=initialize an instance object|Initialize=] |instanceObject| from |module| and |instance|.\n                If this throws an exception, catch it, [=reject=] |promise| with the exception, and terminate these substeps."
    ->
    s"1.  [=initialize an instance object|Initialize=] |instanceObject| from |module| and |instance|.\n                If this throws an exception, catch it, [=reject=] ${ofTypePromise("Instance")} |promise| with the exception, and terminate these substeps.",
    "1. [=Resolve=] |promise| with |instanceObject|."
    ->
    s"1. [=Resolve=] ${ofTypePromise("Instance")} |promise| with |instanceObject|.",

    // #11 (spec inconsistency, docs/spec_inconsistencies.md #4) — links the
    // two unlinked `reject`s (parseable now via
    // ExprParser.NewExceptionExpr's "a {{X}} exception" rule and, for the
    // first site, InstrParser's bare "and return" rule), and annotates with
    // the type, same as #10.
    "1. If |module| is [=error=], reject |promise| with a {{CompileError}} exception and return."
    ->
    s"1. If |module| is [=error=], [=reject=] ${ofTypePromise("Module")} |promise| with a {{CompileError}} exception and return.",
    "1. If [=validate builtins and imported string for a WebAssembly module|validating builtins and imported strings=] for |module| with |builtinSetNames| and |importedStringModule| is false, reject |promise| with a {{CompileError}} exception."
    ->
    s"1. If [=validate builtins and imported string for a WebAssembly module|validating builtins and imported strings=] for |module| with |builtinSetNames| and |importedStringModule| is false, [=reject=] ${ofTypePromise("Module")} |promise| with a {{CompileError}} exception.",

    // #12 (spec bug, suggestion, and hardcoding) — three fixes to `react`
    // (webidl/index.bs) and its 2 call sites in js-api/index.bs:
    //
    //   - (spec bug) unlike `a new promise` (which takes an explicit |realm|
    //     parameter, see #4), `react` tries to derive the realm for its
    //     returned promise capability by chasing |promise|.[[Promise]].[[Realm]]
    //     off the promise being reacted to. But {{Promise}} instances are
    //     ordinary objects, and ordinary objects carry no [[Realm]] internal
    //     slot in ECMA-262 (only function objects do) — so there's no such slot
    //     to chase in the first place. Give `react` the same explicit `|realm|`
    //     parameter `a new promise` already has, and source the {{Promise}}
    //     constructor from it directly instead of trying to re-derive it from
    //     |promise|.
    //   - (suggestion) `react` also declares a leading type parameter T, same
    //     as `a new promise`/`resolve`/`reject` (see #4/#10/#11), elided at
    //     every call site; supplied explicitly here too, same as #10.
    //   - (hardcoding) each call site's `[=React=] to |X|:` step nests its
    //     fulfilled/rejected continuations as bulleted `* If |X| was
    //     fulfilled...:` / `* If |X| was rejected...:` branches directly
    //     beneath it — a shape nothing in this project parses specially, so
    //     the branches were silently dropped rather than compiled (`Perform`'s
    //     `body` never got read back out of them). Rewritten as two named
    //     closures per call site (`Let |onFulfilledSteps| be the following
    //     steps given argument |V|: ...`), passed to `[=React=]` explicitly
    //     via `with |A|, and |B|`, mirroring the closure style `react`'s own
    //     definition already uses for its `onFulfilled`/`onRejected`
    //     `CreateBuiltinFunction` steps (see #8) — a convention
    //     `ExprParser.StepsClosurePrefix` and `ExpandFollowingStepsPass`
    //     already know how to hoist into real closures. Every referenced
    //     closure name must be pipe-delimited (`|onFulfilledSteps|`, not
    //     `onFulfilledSteps`) like every other variable reference in this
    //     file — `ExprParser.parseArgs` only recognizes the piped form and
    //     silently drops anything else, the same way it drops stray prose
    //     words like "with"/"and". `react`'s own definition is patched to
    //     match on the receiving end: its vague "given one or two sets of
    //     steps to perform" prose (never a real parameter — nothing a caller
    //     could actually supply a value for) is replaced with two new
    //     explicit pipe params, `|onFullfilledStepsArg|` and
    //     `|onRejectedStepsArg|`, so the closures passed at each call site
    //     above have somewhere to bind. Its body's two "If there is a set of
    //     steps to be run if the promise was fulfilled/rejected" checks —
    //     previously untestable prose, since "a set of steps" named nothing —
    //     are rewritten to test the corresponding param directly
    //     (`|onFullfilledStepsArg| is not undefined` / `|onRejectedStepsArg|
    //     is not undefined`).
    "to a <code><a interface>Promise</a>&lt;|T|&gt;</code> |promise|, given one or two sets of steps"
    ->
    "to a <code><a interface>Promise</a>&lt;|T|&gt;</code> |promise|, |onFullfilledStepsArg|, and |onRejectedStepsArg| in a [=realm=] |realm|, ",
    """        1.  If there is a set of steps to be run if the promise was fulfilled, then let |result| be
      #            the result of performing them, given |value| if |T| is not {{undefined}}. Otherwise, let
      #            |result| be |value|.""".stripMargin('#')
    ->
    """        1. If |onFullfilledStepsArg| is not undefined and |T| is not {{undefined}}, then let |result| be
      #           the result of performing |onFullfilledStepsArg| given |value|. Otherwise, let
      #           |result| be |value|.""".stripMargin('#'),
    """        1.  If there is a set of steps to be run if the promise was rejected, then let |result| be
      #            the result of performing them, given |reason|. Otherwise, let |result| be
      #            [=a promise rejected with=] |reason|.""".stripMargin('#')
    ->
    """        1.  If |onRejectedStepsArg| is not undefined, then let |result| be
      #            the result of performing |onRejectedStepsArg| given |reason|. Otherwise, let |result| be
      #            [=a promise rejected with=] |reason|.""".stripMargin('#'),
    "1.  Let |constructor| be |promise|.\\[[Promise]].\\[[Realm]].\\[[Intrinsics]].\\[[{{%Promise%}}]]."
    ->
    "1.  Let |constructor| be |realm|.\\[[Intrinsics]].\\[[{{%Promise%}}]].",
    """    1. Let |promise| be [=a new promise=].
      #    1. [=React=] to |promiseOfModule|:
      #        * If |promiseOfModule| was fulfilled with value |module|:
      #            1. [=asynchronously instantiate a WebAssembly module|Instantiate the WebAssembly module=] |module| importing |importObject|, and let |innerPromise| be the result.
      #            1. [=React=] to |innerPromise|:
      #                * If |innerPromise| was fulfilled with value |instance|.
      #                    1. Let |result| be the {{WebAssemblyInstantiatedSource}} value «[ "{{WebAssemblyInstantiatedSource/module}}" → |module|, "{{WebAssemblyInstantiatedSource/instance}}" → |instance| ]».
      #                    1. [=Resolve=] |promise| with |result|.
      #                * If |innerPromise| was rejected with reason |reason|:
      #                    1. [=Reject=] |promise| with |reason|.
      #        * If |promiseOfModule| was rejected with reason |reason|:
      #            1. [=Reject=] |promise| with |reason|.""".stripMargin('#')
    ->
    s"""    1. Let |promise| be [=a new promise=] ${ofTypePromise(
      "WebAssemblyInstantiatedSource",
    )} in the [=current Realm=].
       #    1. Let |onFullfilledSteps| be the following steps given argument |module|:
       #        1. [=asynchronously instantiate a WebAssembly module|Instantiate the WebAssembly module=] |module| importing |importObject|, and let |innerPromise| be the result.
       #        1. Let |innerOnFullfilledSteps| be the following steps given argument |instance|:
       #            1. Let |result| be the {{WebAssemblyInstantiatedSource}} value «[ "{{WebAssemblyInstantiatedSource/module}}" → |module|, "{{WebAssemblyInstantiatedSource/instance}}" → |instance| ]».
       #            1. [=Resolve=] ${ofTypePromise(
      "WebAssemblyInstantiatedSource",
    )} |promise| with |result|.
       #        1. Let |innerOnRejectedSteps| be the following steps given argument |reason|:
       #            1. [=Reject=] ${ofTypePromise(
      "WebAssemblyInstantiatedSource",
    )} |promise| with |reason|.
       #        1. [=React=] ${ofTypePromise(
      "WebAssemblyInstantiatedSource",
    )} to |innerPromise| with |innerOnFullfilledSteps|, and |innerOnRejectedSteps| in the [=current Realm=].
       #    1. Let |onRejectedSteps| be the following steps given argument |reason|:
       #        1. [=Reject=] ${ofTypePromise(
      "WebAssemblyInstantiatedSource",
    )} |promise| with |reason|.
       #    1. [=React=] ${ofTypePromise(
      "WebAssemblyInstantiatedSource",
    )} to |promiseOfModule| with |onFullfilledSteps|, and |onRejectedSteps| in the [=current Realm=]."""
      .stripMargin('#'),

    // #13 (spec bug) — a host function's "name" (used for
    // Function.prototype.name/.length-style introspection) was derived
    // differently depending on whether |funcaddr| pointed at a host function
    // or a module-defined one, because a host function's `funcinst` used to
    // be shaped `{type, hostcode hostfunc}` — no `module` field at all — so
    // `name of the WebAssembly function` branched on that shape, and `read
    // the imports` had to separately track "index of the host function"
    // (a host function's position among |imports|) since it couldn't fall
    // back to the module-defined path's |funcaddrs| lookup. The underlying
    // Wasm Core Spec's `funcinst` representation has since changed so that
    // both kinds of function instance carry a `module` field — the branch
    // (and the index-of-the-host-function tracking that only existed to feed
    // it) is now dead code that js-api/index.bs never updated to drop.
    "                1. [=Create a host function=] from |v| and |functype|, and let |funcaddr| be the result.\n                1. Let |index| be the number of external functions in |imports|. This value |index| is known as the <dfn>index of the host function</dfn> |funcaddr|.\n            1. Let |externfunc| be the [=external value=] [=external value|func=] |funcaddr|."
    ->
    "                1. [=Create a host function=] from |v| and |functype|, and let |funcaddr| be the result.\n            1. Let |externfunc| be the [=external value=] [=external value|func=] |funcaddr|.",
    "    1. If |funcinst| is of the form {type <var ignore>functype</var>, hostcode |hostfunc|},\n        1. Assert: |hostfunc| is a JavaScript object and [$IsCallable$](|hostfunc|) is true.\n        1. Let |index| be the [=index of the host function=] |funcaddr|.\n    1. Otherwise,\n        1. Let |moduleinst| be |funcinst|.module.\n        1. Assert: |funcaddr| is contained in |moduleinst|.funcaddrs.\n        1. Let |index| be the index of |moduleinst|.funcaddrs where |funcaddr| is found."
    ->
    "    1. Let |moduleinst| be |funcinst|.module.\n    1. Assert: |funcaddr| is contained in |moduleinst|.funcaddrs.\n    1. Let |index| be the index of |moduleinst|.funcaddrs where |funcaddr| is found.",

    // #15 (spec inconsistency, docs/spec_inconsistencies.md #2) — normalizes
    // the "external value" family's 4 non-tag variants from Bikeshed
    // pipe-display aliasing (`[=external value|func=]`) to the `for`-scoped
    // form its 5th variant, `tag`, already correctly uses
    // (`[=external value/func=]`).
    "[=external value|func=]" -> "[=external value/func=]",
    "[=external value|global=]" -> "[=external value/global=]",
    "[=external value|mem=]" -> "[=external value/mem=]",
    "[=external value|table=]" -> "[=external value/table=]",

    // #16 (spec inconsistency, docs/spec_inconsistencies.md #2) — the other
    // half of #15: registers the missing `func`/`global`/`mem`/`table`
    // sub-terms in "external value"'s link-defaults block (only `tag` was
    // registered), so the `for`-scoped links #15 normalizes the prose to
    // actually resolve.
    "    url: exec/runtime.html#syntax-externval\n        text: external value\n        for: external value\n            text: tag"
    ->
    "    url: exec/runtime.html#syntax-externval\n        text: external value\n        for: external value\n            text: func\n            text: global\n            text: mem\n            text: table\n            text: tag",

    // #17 (spec bug, docs/spec_errors.md #12) — `|options|["builtins"]`/
    // `|options|["importedStringConstants"]` are indexed unconditionally with
    // no `[=map/exists=]` guard, even though both are optional with no
    // default. Guards both, falling back to an empty list / null
    // respectively.
    """1. Let |builtinSetNames| be |options|["builtins"]."""
    ->
    """1. If |options|["builtins"] [=map/exists=], let |builtinSetNames| be |options|["builtin"]; otherwise, let |builtinSetNames| be « ».""",
    """1. Let |importedStringModule| be |options|["importedStringConstants"]."""
    ->
    """1. If |options|["importedStringConstants"] [=map/exists=], let |importedStringModule| be |options|["builtin"]; otherwise, let |importedStringModule| be null.""",

    // #18 (spec inconsistency, docs/spec_inconsistencies.md #9) — refers back
    // to |x| with the pronoun "it" instead of repeating the pipe-var, which
    // ExprParser/CondParser can't resolve to a variable.
    "1.  If |x| is not given, then let it be the {{undefined}} value."
    ->
    "1.  If |x| is not given, then let |x| be the {{undefined}} value.",

    // #19 (spec bug) — "is of the form [=external-type/tag=] |attribute|
    // <var ignore>functype</var>" binds an |attribute| that no longer exists
    // in the runtime representation: `al_of_tagtype`/`TagT` (construct.ml)
    // wrap a tag's typeuse directly, with no separate attribute-kind field,
    // because "exception" is still the only tag attribute this proposal
    // defines. The very next step only asserts |attribute| always equals
    // that one constant, then never uses it again — a vestigial binding left
    // behind from before the representation dropped the field, same as #13.
    // Dropped at both of this form's occurrences (js-api/index.bs:540, 579;
    // `.replace` matches both since the text is byte-identical).
    "1. If |externtype| is of the form [=external-type/tag=] |attribute| <var ignore>functype</var>,\n            1. Assert: |attribute| is [=tagtype/attribute/exception=]."
    ->
    "1. If |externtype| is of the form [=external-type/tag=] <var ignore>functype</var>,",

    // #14 (spec bug) — "Let [|parameters|] → [|results|] be |functype|."
    // destructures |functype| as if it already were its own underlying
    // comptype (a flat params/results pair), but |functype| is a deftype
    // (e.g. `func_type`'s own declared return type, or an imported
    // function's externtype payload) — per the Wasm Core Spec's Embedding
    // API (embedding.rst's func_type post-condition: "the returned defined
    // type ... expands to a function type"), the caller is responsible for
    // expanding it first via the $Expand relation. Every occurrence in this
    // file elides that step; made explicit here via the wjmeta-bridge's
    // `expand` convenience (see docs/esmeta_errors.md's sibling note on
    // spectec's own Embedding.expand). All 3 occurrences share byte-identical
    // trailing text, so one replacement covers all of them.
    "be |functype|."
    -> "be [=expand=](|functype|).",

    // #20 (spec inconsistency, docs/spec_inconsistencies.md #3) — brackets
    // the bare `memory address` dfn-link to match its other 6 occurrences.
    // `TypeAnnotatedPrefix` (ExprParser) now accepts a trailing `|var|...`
    // EXPR (not just a `[=...=]`-led one), so "the [=TERM=] EXPR" still
    // parses to just EXPR — TERM dropped as a pure type annotation, same
    // idiom as every other TypeAnnotatedPrefix use.
    "Let |memaddr| be the memory address |frame|.[=frame/module=].[=moduleinst/memaddrs=][|x|]."
    -> "Let |memaddr| be the [=memory address=] |frame|.[=frame/module=].[=moduleinst/memaddrs=][|x|].",

    // #21 (spec bug) — `name of the WebAssembly function` (index.bs:1254-1255)
    // reads a moduleinst's function-address list as `|moduleinst|.funcaddrs`,
    // but the Wasm Core Spec's actual runtime `moduleinst` record names that
    // field `FUNCS` (see `Ds.Store`/`Construct`'s moduleinst construction) —
    // there is no `funcaddrs` field at all. This exact fragment occurs
    // nowhere else in the file, so one small replacement (rather than
    // anchoring a whole indentation-sensitive block, fragile against #13
    // above reformatting this same text before this patch runs) covers both
    // of its occurrences.
    "|moduleinst|.funcaddrs" -> "|moduleinst|.funcs",

    // #22 (hardcoding) — "create a new Exported Function" (index.bs:1265)
    // defines its `CreateBuiltinFunction` `behaviour` argument as a quoted
    // sentence linking to a separately-defined algorithm ("call an Exported
    // Function"), rather than any of the closure idioms this pipeline already
    // parses. This is a legitimate form — ECMA-262's own `CreateBuiltinFunction`
    // explicitly allows `behaviour` to be "an Abstract Closure, a set of
    // algorithm steps, or *some other definition of a function's behaviour
    // provided in this specification*" — just one this pipeline doesn't
    // structurally recognize yet. Rewritten into the "the following steps
    // given the list of arguments V:" idiom (ExprParser's
    // `VariadicStepsClosurePrefix`) rather than the plain "given argument V:"
    // one: `argValues` is "a list of JavaScript arguments" (index.bs:1279,
    // `call an Exported Function`'s own declared param) — a WebAssembly
    // function's arity is dynamic, so it can't be described as N
    // individually-named positional parameters the way a genuine single value
    // (e.g. `react`'s `onFulfilledSteps given argument V`) can; it needs to
    // bind the *entire* arguments list itself.
    "Let |steps| be \"[=call an Exported Function|call the Exported Function=] |funcaddr| with arguments.\""
    ->
    """Let |steps| be the following steps given the list of arguments |argValues|:
        1. Return the result of [=call an Exported Function=] with |funcaddr| and |argValues|.""",

    // #23 (hardcoding) — "call an Exported Function" (index.bs:1296) throws
    // an untyped "an exception" when `func_invoke` fails, then a *separate*
    // sentence ("This exception should be a WebAssembly {{RuntimeError}}
    // exception, unless otherwise indicated by the WebAssembly error
    // mapping.") clarifies what type it should actually be. AlgorithmExtractor
    // splits that clarifying sentence off as its own sibling step rather than
    // folding it into the same `If`, so it runs unconditionally regardless of
    // whether |ret| was actually an error — and even if it didn't, an untyped
    // `Throw("an exception")` doesn't match CompletionWrapping's `ThrowTarget`
    // (only "a {{X}} exception" does) and would itself fall through to an
    // unimplemented placeholder. Rewritten into a single concrete
    // `throw a {{RuntimeError}} exception.` step, dropping the "unless
    // otherwise indicated by the WebAssembly error mapping" escape hatch —
    // no current spec text needs a WebAssembly failure to surface as anything
    // other than a RuntimeError.
    "throw an exception. This exception should be a WebAssembly {{RuntimeError}} exception, unless otherwise indicated by <a href=\"#errors\">the WebAssembly error mapping</a>."
    ->
    "throw a {{RuntimeError}} exception.",

    // #24 (spec inconsistency, docs/spec_inconsistencies.md #5) — missing "of
    // the form" before `[=exception=] |exnaddr|` makes `CondParser` fall
    // through to its default `Eq` handling instead of a destructuring match,
    // reading `|exnaddr|` as if already bound instead of binding it —
    // crashes with "unknown variable: exnaddr".
    "If |ret| is [=exception=] |exnaddr|, then"
    ->
    "If |ret| is of the form [=exception=] |exnaddr|, then",

    // #25 (spec inconsistency, docs/spec_inconsistencies.md #1) — ToJSValue's
    // ref.i31 case omits "interpreted as a [=mathematical value=]" that its 4
    // sibling cases all include; without it, `𝔽(|i31|)` receives a still
    // wasm-wrapped value instead of `AsMath`'s converted one.
    "1. Return [=𝔽=](|i31|)."
    ->
    "1. Return [=𝔽=](|i31| interpreted as a [=mathematical value=]).",

    // #26 (spec bug, docs/spec_errors.md #14) — every call site prefixes
    // `ToJSValue` with `[=!=]` (ReturnIfAbrupt) even though it never returns
    // a Completion Record. Strips it from the 7 sites where `!` targets
    // `ToJSValue` directly (index.bs:1096, 1214, 1302, 1308, 1312, 1327,
    // 1762); leaves the other 5 (index.bs:1934, 1943, 2048, 2080, 2123)
    // alone, where `!` legitimately targets `$Call$` and `ToJSValue(...)` is
    // merely passed in as an argument.
    "[=!=] [=ToJSValue=]"
    ->
    "[=ToJSValue=]",

    // #27 (spec inconsistency, docs/spec_inconsistencies.md #6) — "the
    // [=mem_size=](...)" has a leading "the " every other embedding-function
    // call in this document omits, which pushes it out of the extractor's
    // call-syntax pattern (requires the link to start the expression) and
    // silently drops the call.
    "1. Let |ret| be the [=mem_size=](|store|, |memaddr|)."
    ->
    "1. Let |ret| be [=mem_size=](|store|, |memaddr|).",

    // #28 (spec inconsistency, docs/spec_inconsistencies.md #7) — "create a
    // host function"'s closure handles the associated store ambiently
    // instead of receiving/returning it explicitly like every other
    // store-touching algorithm here. Gives it an explicit |state| parameter,
    // synced into the ambient field as its first step — only the boundary
    // crossing needed fixing, so the rest of the closure body still reads
    // the field directly.
    "1. Let |hostfunc| be a [=host function=] which performs the following steps when called with arguments |arguments|:\n        1. Let |realm| be |func|'s [=associated Realm=]."
    ->
    "1. Let |hostfunc| be a [=host function=] which performs the following steps when called with state |state| and arguments |arguments|:\n        1. Set the [=surrounding agent=]'s [=associated store=] to |state|.\n        1. Let |realm| be |func|'s [=associated Realm=].",

    // #29 (spec inconsistency, docs/spec_inconsistencies.md #7) — the other
    // half of #28: the closure's success path returns just
    // |result|.\[[Value]], dropping the store instead of pairing it up like
    // every other store-touching algorithm here. |store| is already freshly
    // re-read just above — reuse it directly in a real pair.
    "1. Otherwise, return |result|.\\[[Value]]."
    ->
    "1. Otherwise, return (|store|, |result|.\\[[Value]]).",

    // #30 (spec inconsistency, docs/spec_inconsistencies.md #8) —
    // `ToWebAssemblyValue`'s two `[=match_valtype=](...)` conditions
    // (index.bs:1450,1453) are the only boolean-function-call conditions in
    // this document not spelled out with "is true"/"is false" (see #8 for the
    // full survey). Adds the missing "is true".
    "[=heap-type/extern=])," -> "[=heap-type/extern=]) is true,",
    "[=heap-type/func=])," -> "[=heap-type/func=]) is true,",

    // #31 (spec inconsistency, docs/spec_inconsistencies.md #10) — the only
    // `Let |var| be ...` in this file that annotates a plain scalar with a
    // type link. `ExprParser` reads bare "[=link=] |var|" (space, no parens)
    // as a call (`LinkProse`) wherever it appears, so this Let's LHS parses
    // as `AlgoCall`, not `Var` — `Compiler` reports it as "unsupported Let
    // lhs". Dropped outright rather than reformatted, since nothing here
    // needs the link kept: unlike the 5 `external value|X` Let-LHS sites
    // (line 561/566/571/576/582), which destructure a genuine SpecTec
    // tagged-union value and so already work via `ExpandDestructuringLetPass`,
    // |hostaddr| here is just a plain integer with no runtime tag of its own.
    "Let [=host address=] |hostaddr| be the smallest address"
    -> "Let |hostaddr| be the smallest address",

    // #32 (spec bug, docs/spec_errors.md #15) — stale phrasing left over
    // from an older Wasm Core Spec revision, where the `ref.null` runtime
    // value still carried its heap type. The current spec's `ref` value
    // grammar (`4.1-execution.values.spectec`) dropped that: `REF.NULL_ADDR`
    // is a bare nullary constructor, always typed as the bottom heap type
    // regardless of context (`s |- REF.NULL_ADDR : REF NULL BOT`) — unlike
    // every sibling `[=ref.X=]` value link in this file (func/host/i31/
    // struct/array/extern, all linking the same `exec/runtime.html#values`
    // target), whose argument genuinely is part of the value. Both
    // occurrences drop the now-nonexistent heaptype argument.
    "[=ref.null=] <var ignore>t</var>, return null."
    -> "[=ref.null=], return null.",
    "1. Let |r| be [=ref.null=] |heaptype|."
    -> "1. Let |r| be [=ref.null=].",

    // #33 (spec bug, docs/spec_errors.md #16) — `[=!=]` (ReturnIfAbrupt)
    // prefixes two ECMA-262 AO calls whose own declared signatures never
    // wrap a result in a Completion Record: `OrdinaryObjectCreate` ("returns
    // an Object", 3 call sites) and `CreateBuiltinFunction` ("returns a
    // built-in function object", 1 call site, in `a new Exported Function`)
    // — the same class of mistake as #26's `ToJSValue`.
    "[=!=] [$OrdinaryObjectCreate$]"
    ->
    "[$OrdinaryObjectCreate$]",
    "[=!=] [$CreateBuiltinFunction$]"
    ->
    "[$CreateBuiltinFunction$]",

    // #34 (spec inconsistency, docs/spec_inconsistencies.md #7) — the last
    // piece of #7/#28/#29: the throw branch is the one remaining exit of
    // `create a host function`'s hostfunc closure that still doesn't follow
    // the (state, result) explicit-threading convention #28/#29 already
    // gave the rest of it — it just imperatively "executes" two Wasm
    // instructions with no `Return` at all. Rewritten as an explicit
    // `Return (store, result)` pair, using the same `result` shape
    // (`(ref.exn a) throw_ref`, spectec's own `result` syntax,
    // `4.0-execution.configurations.spectec`) the spectec submodule's
    // `$callhostfunc` now expects on this exact path. The spaces just inside
    // `« »` are load-bearing, not stylistic — `ExprParser.MapLiteral`
    // matches a bracket touching `«`/`»` on either side (`«[...]»`) before
    // `ListLiteral` ever gets a look, and this list's first/last elements
    // are themselves `[=...=]`-bracketed spec links, so a tight `«[=ref.exn=]
    // ...[=throw_ref=]»` would misparse as a (garbage) map literal instead.
    "1. Execute the WebAssembly instructions ([=ref.exn=] |address|) ([=throw_ref=])."
    ->
    "1. Return (|store|, « [=ref.exn=] |address|, [=throw_ref=] »).",

    // #35 (spec bug, docs/spec_errors.md #9) — `tag_type` has the exact same
    // deftype-returning shape as `func_type` (#9/#14 above), but isn't caught
    // by that patch's literal `"be |functype|."` match (these two both call
    // `[=tag_type=]`, not `|functype|`). Wraps each in `[=expand=]` the same
    // way. Not byte-identical to each other (different `tag_type` arguments),
    // so two separate replacements.
    "1. Let [|types|] → [] be [=tag_type=](|store|, |exceptionTag|.\\[[Address]])."
    ->
    "1. Let [|types|] → [] be [=expand=]([=tag_type=](|store|, |exceptionTag|.\\[[Address]])).",
    "1. Let [|types|] → [] be [=tag_type=](|store|, |tagaddr|)."
    ->
    "1. Let [|types|] → [] be [=expand=]([=tag_type=](|store|, |tagaddr|)).",

    // #36 (spec bug, docs/spec_errors.md #17) — the construction-direction
    // mirror of #35/#9: `tag_alloc`'s second argument is built directly as a
    // bare comptype (SpecTec's `X → Y` arrow notation) where the Embedding
    // API's `tag_alloc(store, tagtype)` expects a real `deftype`. Wraps each
    // in the new `fold` convenience. Not byte-identical to each other
    // (different comptypes), so two separate replacements.
    "[=tag_alloc=](|store|, |wasmParameters| → « »)."
    ->
    "[=tag_alloc=](|store|, [=fold=](|wasmParameters| → « »)).",
    "[=tag_alloc=](|store|, « [=externref=] » → « »)."
    ->
    "[=tag_alloc=](|store|, [=fold=](« [=externref=] » → « »)).",
  )

  def apply(source: String): String =
    patches.foldLeft(source) { case (s, (from, to)) => s.replace(from, to) }
