package esmeta.wji.lang

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
  *
  * REQUIRED, every time you add a *(spec bug)* entry: add a matching numbered
  * section to `docs/spec_errors.md` in the SAME change (title,
  * File/Current/Expected/Reason, same shape as its existing entries).
  * `docs/spec_errors.md` is what actually gets reported upstream to the spec
  * authors — a patch added here without a matching entry there never reaches
  * them and just silently rots as tribal knowledge in this file. Do not defer
  * this to a follow-up commit; do it now, in the same patch that adds the entry
  * below.
  *   - *(suggestion)* makes an already-valid elision explicit — a spot where
  *     Bikeshed prose conventionally omits an argument (a generic type
  *     parameter, an implicit realm) that this project's extractor/compiler
  *     needs spelled out in order to compile. Nothing here is wrong in the spec
  *     as written; it's just not explicit enough for this tool.
  *   - *(hardcoding)* invents a call convention with no literal counterpart in
  *     the spec prose at all — needed where this project can't yet parse a
  *     shape the spec actually uses, rather than just an elided argument.
  *     Currently only `react`'s call sites (see #12): the spec nests a call's
  *     fulfilled/rejected continuations as bulleted branches directly under
  *     `[=React=] to |X|:`, a shape nothing in this project parses specially,
  *     so they're rewritten as two named closures passed explicitly instead.
  *
  * The *(suggestion)* patches all serve one of two purposes, each showing up
  * folded into several numbered entries below rather than as a pair of its own,
  * so it's called out here instead of at every site:
  *
  *   - *current realm* — `a new promise` and `react` (webidl/index.bs) each
  *     need the calling realm threaded through explicitly: {{Promise}}
  *     instances are ordinary objects, and ordinary objects carry no [[Realm]]
  *     internal slot in ECMA-262 (only function objects do), so there's no slot
  *     for `react` to derive it from, and `a new promise` simply declares the
  *     parameter without every caller supplying it. Fixed in #4 (`a new
  *     promise`'s 3 call sites) and #12 (`react`'s own signature plus its 2
  *     call sites).
  *   - *type parameter* — `a new promise`, `resolve`, `react`, and `reject`
  *     each declare a leading type parameter T (see
  *     `AlgorithmExtractor.GenericVarIgnore`), but every real call site elides
  *     it. Fixed in #4, #10, #11, and #12, each supplying the type of the
  *     `promise`/`p` argument being acted on at that site.
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

    // #3 (TODO: this is temporary, and should be removed)
    // esmeta doesn't support overloaded methods, but {{WebAssembly}}'s
    // `instantiate` is overloaded (module-first and buffer-source-first
    // variants). Rename this one to avoid the collision.
    "The <dfn method for=\"WebAssembly\">instantiate(|moduleObject|, |importObject|)</dfn> method, when invoked, performs the following steps:"
    ->
    "The <dfn method for=\"WebAssembly\">instantiate_object(|moduleObject|, |importObject|)</dfn> method, when invoked, performs the following steps:",

    // #4 (spec bug and suggestion) — `a new promise` is defined with an explicit |realm|
    // parameter and type parameter T (webidl/index.bs), but js-api/index.bs
    // calls it with no argument, eliding the realm the way Bikeshed prose
    // conventionally does, and no type, eliding what the promise resolves
    // with. Make both explicit so it compiles and so the type flows into the
    // call. All 3 call sites open with the same "1. Let |promise| be [=a new
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

    // #11 (spec bug) — the compile algorithm's two CompileError rejections
    // are written as plain "reject ... exception" prose with no [=...=] link,
    // so they don't parse as a call to `reject` at all today. Bracket them
    // into real links (now parseable thanks to ExprParser.NewExceptionExpr's
    // "a {{X}} exception" rule and, for the first site, InstrParser's bare
    // "and return" rule) and annotate with the type, same as #10.
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

    // #15 (spec bug) — the "external value" family's 4 non-tag variants
    // (func/global/mem/table) are written `[=external value|X=]` (Bikeshed
    // pipe-display aliasing) instead of the `for`-scoped `[=external
    // value/X=]` form its 5th variant, tag, already correctly uses (line
    // ~220's link-defaults block declares `for: external value` / `text: tag`
    // — but never registers func/global/mem/table the same way, unlike the
    // parallel "external-type" block a few lines below it, which does
    // register all 5 of *its* variants `for: external-type`). Normalized to
    // the `for`-scoped form for consistency with `tag` and with
    // `external-type`'s own 5 variants.
    "[=external value|func=]" -> "[=external value/func=]",
    "[=external value|global=]" -> "[=external value/global=]",
    "[=external value|mem=]" -> "[=external value/mem=]",
    "[=external value|table=]" -> "[=external value/table=]",

    // #16 (spec bug) — the link-defaults block that registers "external
    // value"'s linkable sub-terms (see #15's reasoning) only ever registers
    // `tag`, never `func`/`global`/`mem`/`table` — unlike the parallel
    // "external-type" block just below it, which registers all 5 of its own
    // variants `for: external-type`. This is the root cause #15 patches
    // around (prose can't validly link `[=external value/func=]` etc.
    // without a matching registered anchor) — registering the missing 4 here
    // too, so the anchors actually back the `for`-scoped links #15
    // normalizes the prose to.
    "    url: exec/runtime.html#syntax-externval\n        text: external value\n        for: external value\n            text: tag"
    ->
    "    url: exec/runtime.html#syntax-externval\n        text: external value\n        for: external value\n            text: func\n            text: global\n            text: mem\n            text: table\n            text: tag",

    // # 17 (spec bug) -- it needs to explicitly handle the case where builtins and importedStringConstants are omited. (see https://webidl.spec.whatwg.org/#example-f7efabfd)
    """1. Let |builtinSetNames| be |options|["builtins"]."""
    ->
    """1. If |options|["builtins"] [=map/exists=], let |builtinSetNames| be |options|["builtin"]; otherwise, let |builtinSetNames| be « ».""",
    """1. Let |importedStringModule| be |options|["importedStringConstants"]."""
    ->
    """1. If |options|["importedStringConstants"] [=map/exists=], let |importedStringModule| be |options|["builtin"]; otherwise, let |importedStringModule| be null.""",

    // #18 hardcoded patch for non-normative style
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

    // #18 (spec bug) — "Let [|parameters|] → [|results|] be |functype|."
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

    // #20 (spec bug) — "the memory address |frame|.[=frame/module=]..."
    // (`memory.grow`, index.bs:929) writes the [=memory address=] dfn-link
    // bare, unlike every other of its 6 occurrences in this file, which all
    // bracket it. Bracketed here to match; `TypeAnnotatedPrefix` (ExprParser)
    // now accepts a trailing `|var|...` EXPR (not just a `[=...=]`-led one),
    // so "the [=TERM=] EXPR" still parses to just EXPR — TERM is dropped as a
    // pure type annotation, same idiom as every other TypeAnnotatedPrefix use.
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
  )

  def apply(source: String): String =
    patches.foldLeft(source) { case (s, (from, to)) => s.replace(from, to) }
