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
    // buffer-source-first variants). Renames *both* to avoid the collision —
    // a naming distinction the spec itself doesn't make, invented purely to
    // route around this project's lack of overload support. Freeing up the
    // literal `instantiate` name this way (rather than just the module-first
    // one, as before) lets `AddInstantiateOverloadDispatchPass` claim it for a
    // synthetic third algorithm that dispatches to whichever of these two by
    // checking its first argument's runtime type (`personal/TODO.md` #58,
    // `docs/hardcodes.md` #23).
    "The <dfn method for=\"WebAssembly\">instantiate(|moduleObject|, |importObject|)</dfn> method, when invoked, performs the following steps:"
    ->
    "The <dfn method for=\"WebAssembly\">instantiate_object(|moduleObject|, |importObject|)</dfn> method, when invoked, performs the following steps:",
    "Promise&lt;Instance> instantiate("
    ->
    "Promise&lt;Instance> instantiate_object(",
    "The <dfn method for=\"WebAssembly\">instantiate(|bytes|, |importObject|, |options|)</dfn> method, when invoked, performs the following steps:"
    ->
    "The <dfn method for=\"WebAssembly\">instantiate_bytes(|bytes|, |importObject|, |options|)</dfn> method, when invoked, performs the following steps:",
    "Promise&lt;WebAssemblyInstantiatedSource> instantiate("
    ->
    "Promise&lt;WebAssemblyInstantiatedSource> instantiate_bytes(",

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
    """1. Let |promise| be [=a new promise=].
      #    1. Run the following steps [=in parallel=]:""".stripMargin('#')
    ->
    s"""1. Let |promise| be [=a new promise=] ${ofTypePromise(
      "Module",
    )} in the [=current Realm=].
      #    1. Run the following steps [=in parallel=]:""".stripMargin('#'),
    """1. Let |promise| be [=a new promise=].
      #    1. Let |module| be |moduleObject|.\[[Module]].""".stripMargin('#')
    ->
    s"""1. Let |promise| be [=a new promise=] ${ofTypePromise(
      "Instance",
    )} in the [=current Realm=].
      #    1. Let |module| be |moduleObject|.\\[[Module]].""".stripMargin('#'),

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
    """<div algorithm>
      #    The getter of the <dfn attribute for="Global">value</dfn>"""
      .stripMargin('#')
    ->
    """<div algorithm="global-value-getter">
      #    The getter of the <dfn attribute for="Global">value</dfn>"""
      .stripMargin('#'),
    """
      #
      #    The setter of the value attribute of {{Global}}""".stripMargin('#')
    ->
    """
      #</div>
      #
      #<div algorithm="global-value-setter">
      #    The setter of the value attribute of {{Global}}""".stripMargin('#'),

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
    """1.  [=Instantiate the core of a WebAssembly module=] |module| with |imports|, and let |instance| be the result.
      #                If this throws an exception, catch it, [=reject=] |promise| with the exception, and terminate these substeps."""
      .stripMargin('#')
    ->
    s"""1.  [=Instantiate the core of a WebAssembly module=] |module| with |imports|, and let |instance| be the result.
      #                If this throws an exception, catch it, [=reject=] ${ofTypePromise(
      "Instance",
    )} |promise| with the exception, and terminate these substeps."""
      .stripMargin('#'),
    """1.  [=initialize an instance object|Initialize=] |instanceObject| from |module| and |instance|.
      #                If this throws an exception, catch it, [=reject=] |promise| with the exception, and terminate these substeps."""
      .stripMargin('#')
    ->
    s"""1.  [=initialize an instance object|Initialize=] |instanceObject| from |module| and |instance|.
      #                If this throws an exception, catch it, [=reject=] ${ofTypePromise(
      "Instance",
    )} |promise| with the exception, and terminate these substeps."""
      .stripMargin('#'),
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

    // #13 (spec inconsistency, docs/spec_inconsistencies.md #11) — a host
    // function's "name" (used for Function.prototype.name/.length-style
    // introspection) needs its position among the module's function-typed
    // imports, computed once when the host function is created (`read the
    // imports`) and needed again later, keyed by |funcaddr|, by an unrelated
    // algorithm (`name of the WebAssembly function`). The original prose
    // wrote this as an ad-hoc "Let |index| be N. This value |index| is known
    // as the index of the host function |funcaddr|." — an inline dfn
    // introduced mid-algorithm, a shape nothing else in this document uses
    // and this project doesn't mechanize. Every structurally identical need
    // elsewhere in this same document (funcaddr -> JS object, keyed lookup
    // from a completely different algorithm) instead uses an explicit
    // "surrounding agent's associated <ordered map/list>" (see the "Exported
    // Function cache" family, index.bs:346-353) — rewritten here the same
    // way, using a plain list rather than a map since the value being
    // recovered later isn't a cached object, just a position, and "the index
    // of X where Y is found" (already used by the sibling module-defined
    // branch just below) recovers it without needing map/exists+map/Set.
    // Also fixes the host/module-defined dispatch itself: the original `If
    // |funcinst| is of the form {type functype, hostcode |hostfunc|}`
    // pattern-matches a funcinst shape the Wasm Core Spec no longer has —
    // funcinst is uniformly {TYPE deftype, MODULE moduleinst, CODE funccode}
    // now (4.4-execution.modules.spectec:74-75), so this dispatches on
    // |funcinst|.code's own HOSTFUNC-vs-FUNC shape instead
    // (funccode = func | hostfunc, 4.0-execution.configurations.spectec:60-61).
    // The dropped "Assert: |hostfunc| is a JavaScript object..." step is
    // dropped for the same reason: a host function's CODE payload is an
    // opaque id (`HOSTFUNC text`), not the JS object itself, so asserting
    // that no longer makes sense either. Both branches converge on binding
    // |funcaddrs| to just that one list — the sanity-check Assert (still
    // meaningful either way: a funcaddr found this way is always actually in
    // that list, by construction) and the final index lookup are each
    // written once, after the branch, instead of duplicated per branch.
    // Writes `|funcinst|.module.funcs` directly (chained field access,
    // already a general `ExprParser`/`DotField` capability, no `|moduleinst|`
    // intermediate needed) rather than `.funcaddrs` — this rewrite already
    // produces the field name `docs/spec_errors.md` #10 separately fixes, so
    // that patch's own `"|moduleinst|.funcaddrs"` text no longer occurs here
    // for it to match; #10 stays (still an accurate, reportable bug against
    // the *unpatched* spec text) but is now a no-op for this occurrence.
    "    1. Let |index| be the number of external functions in |imports|. This value |index| is known as the <dfn>index of the host function</dfn> |funcaddr|."
    ->
    "1. [=list/Append=] |funcaddr| to the [=surrounding agent=]'s associated [=Function Import List=].",
    """    1. If |funcinst| is of the form {type <var ignore>functype</var>, hostcode |hostfunc|},
      #        1. Assert: |hostfunc| is a JavaScript object and [$IsCallable$](|hostfunc|) is true.
      #        1. Let |index| be the [=index of the host function=] |funcaddr|.
      #    1. Otherwise,
      #        1. Let |moduleinst| be |funcinst|.module.
      #        1. Assert: |funcaddr| is contained in |moduleinst|.funcaddrs.
      #        1. Let |index| be the index of |moduleinst|.funcaddrs where |funcaddr| is found."""
      .stripMargin('#')
    ->
    """    1. If |funcinst|.code is of the form [=hostfunc=] <var ignore>hostfunc</var>,
      #        1. Let |funcaddrs| be the [=surrounding agent=]'s associated [=Function Import List=].
      #    1. Otherwise,
      #        1. Let |funcaddrs| be |funcinst|.module.funcs.
      #    1. Assert: |funcaddr| is contained in |funcaddrs|.
      #    1. Let |index| be the index of |funcaddrs| where |funcaddr| is found."""
      .stripMargin('#'),

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
    """    url: exec/runtime.html#syntax-externval
      #        text: external value
      #        for: external value
      #            text: tag""".stripMargin('#')
    ->
    """    url: exec/runtime.html#syntax-externval
      #        text: external value
      #        for: external value
      #            text: func
      #            text: global
      #            text: mem
      #            text: table
      #            text: tag""".stripMargin('#'),

    // #17 (spec bug, docs/spec_errors.md #12) — `|options|["builtins"]`/
    // `|options|["importedStringConstants"]` are indexed unconditionally with
    // no `[=map/exists=]` guard, even though both are optional with no
    // default. Guards both, falling back to an empty list / null
    // respectively.
    """1. Let |builtinSetNames| be |options|["builtins"]."""
    ->
    """1. If |options|["builtins"] [=map/exists=], let |builtinSetNames| be |options|["builtins"]; otherwise, let |builtinSetNames| be « ».""",
    """1. Let |importedStringModule| be |options|["importedStringConstants"]."""
    ->
    """1. If |options|["importedStringConstants"] [=map/exists=], let |importedStringModule| be |options|["importedStringConstants"]; otherwise, let |importedStringModule| be null.""",

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
    """1. If |externtype| is of the form [=external-type/tag=] |attribute| <var ignore>functype</var>,
      #            1. Assert: |attribute| is [=tagtype/attribute/exception=]."""
      .stripMargin('#')
    ->
    "1. If |externtype| is of the form [=external-type/tag=] <var ignore>functype</var>,",

    // #14 (spec bug, docs/spec_errors.md #9/#18) — "Let [|parameters|] →
    // [|results|] be |functype|." destructures |functype| as if it already
    // were its own underlying comptype (a flat params/results pair), but
    // |functype| is a deftype (e.g. `func_type`'s own declared return type,
    // or an imported function's externtype payload) — per the Wasm Core
    // Spec's Embedding API (embedding.rst's func_type post-condition: "the
    // returned defined type ... expands to a function type"), the caller is
    // responsible for expanding it first via the $Expand relation. Made
    // explicit via the wjmeta-bridge's `expand` convenience. Also drops the
    // `[...]`-bracket decoration around each side and adds the `FUNC`
    // discriminator `.spectec`'s current `comptype` grammar requires
    // (docs/spec_errors.md #18) — `[=comp-type/func=]` (dangling in a real
    // Bikeshed render, since no unscoped `func` dfn exists for `comp-type`,
    // only `for:`-scoped ones for unrelated productions; irrelevant here
    // since this project never actually renders Bikeshed). Two occurrences
    // share byte-identical text (1283, 1323); the third (1269) uses `<var
    // ignore>` for the unused results side instead of a plain `|var|`, so
    // it's a separate replacement.
    "1. Let [|parameters|] → [|results|] be |functype|."
    ->
    "1. Let [=comp-type/func=] |parameters| → |results| be [=expand=](|functype|).",
    "1. Let [|paramTypes|] → [<var ignore>resultTypes</var>] be |functype|."
    ->
    "1. Let [=comp-type/func=] |paramTypes| → <var ignore>resultTypes</var> be [=expand=](|functype|).",

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
    """1. Let |hostfunc| be a [=host function=] which performs the following steps when called with arguments |arguments|:
      #        1. Let |realm| be |func|'s [=associated Realm=].""".stripMargin(
      '#',
    )
    ->
    """1. Let |hostfunc| be a [=host function=] which performs the following steps when called with state |state| and arguments |arguments|:
      #        1. Set the [=surrounding agent=]'s [=associated store=] to |state|.
      #        1. Let |realm| be |func|'s [=associated Realm=].""".stripMargin(
      '#',
    ),

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
    ->
    "[=ref.null=], return null.",
    "1. Let |r| be [=ref.null=] |heaptype|."
    ->
    "1. Let |r| be [=ref.null=].",

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

    // #35 (spec bug, docs/spec_errors.md #9/#18) — `tag_type` has the exact
    // same deftype-returning shape as `func_type` (#9/#14 above), but isn't
    // caught by that patch's literal `"be |functype|."` match (these two both
    // call `[=tag_type=]`, not `|functype|`). Wraps each in `[=expand=]` and
    // adds the `FUNC` discriminator, the same fix and reasoning as #14. Not
    // byte-identical to each other (different `tag_type` arguments), so two
    // separate replacements.
    "1. Let [|types|] → [] be [=tag_type=](|store|, |exceptionTag|.\\[[Address]])."
    ->
    "1. Let [=comp-type/func=] |types| → « » be [=expand=]([=tag_type=](|store|, |exceptionTag|.\\[[Address]])).",
    "1. Let [|types|] → [] be [=tag_type=](|store|, |tagaddr|)."
    ->
    "1. Let [=comp-type/func=] |types| → « » be [=expand=]([=tag_type=](|store|, |tagaddr|)).",

    // #36 (spec bug, docs/spec_errors.md #17/#18) — the construction-direction
    // mirror of #35/#9: `tag_alloc`'s second argument is built directly as a
    // bare comptype (SpecTec's `X → Y` arrow notation) where the Embedding
    // API's `tag_alloc(store, tagtype)` expects a real `deftype`. Wraps each
    // in the new `fold` convenience, and adds the same `FUNC` discriminator as
    // #14/#35. Not byte-identical to each other (different comptypes), so two
    // separate replacements.
    "[=tag_alloc=](|store|, |wasmParameters| → « »)."
    ->
    "[=tag_alloc=](|store|, [=fold=]([=comp-type/func=] |wasmParameters| → « »)).",
    "[=tag_alloc=](|store|, « [=externref=] » → « »)."
    ->
    "[=tag_alloc=](|store|, [=fold=]([=comp-type/func=] « [=externref=] » → « »)).",

    // #37 (spec bug, docs/spec_errors.md #20) — `Tag`/
    // `Exception` are the only two `WebAssembly`-namespace interfaces in this
    // file exposed to just `(Window,Worker,Worklet)`; every other one
    // (`WebAssembly` itself, `Module`, `Instance`, `Memory`, `Table`,
    // `Global`) is `Exposed=*`, deliberately host-agnostic. Normalized to
    // match.
    "[LegacyNamespace=WebAssembly, Exposed=(Window,Worker,Worklet)]\ninterface Tag {"
    ->
    "[LegacyNamespace=WebAssembly, Exposed=*]\ninterface Tag {",
    "[LegacyNamespace=WebAssembly, Exposed=(Window,Worker,Worklet)]\ninterface Exception {"
    ->
    "[LegacyNamespace=WebAssembly, Exposed=*]\ninterface Exception {",

    // #39 (spec inconsistency, docs/spec_inconsistencies.md #12) —
    // AddressValueToU64/U64ToAddressValue compare |addrtype| against raw
    // quoted strings "i32"/"i64", instead of the [=i32=]/[=i64=] link idiom
    // every other numtype/valtype comparison in this document uses (e.g.
    // "If |valtype| is [=i64=]" at line 519, "If |type| is [=i32=]" at line
    // 1427) — structurally the same comparison (both addrtype and
    // valtype/numtype reduce to the same uppercase-Case-tag runtime shape via
    // `al_of_addrtype`/`al_of_numtype`'s identical `nullary` convention), just
    // spelled differently here.
    "1. If |addrtype| is \"i32\"," -> "1. If |addrtype| is [=i32=],",
    "1. If |addrtype| is \"i64\"," -> "1. If |addrtype| is [=i64=],",
    "1. If |addrtype| is \"i32\", return [=𝔽=](|v| interpreted as a [=mathematical value=])."
    ->
    "1. If |addrtype| is [=i32=], return [=𝔽=](|v| interpreted as a [=mathematical value=]).",
    "1. Else if |addrtype| is \"i64\", return [=ℤ=](|v| interpreted as a [=mathematical value=])."
    ->
    "1. Else if |addrtype| is [=i64=], return [=ℤ=](|v| interpreted as a [=mathematical value=]).",

    // #43 (spec bug, docs/spec_errors.md #19) — AddressValueToU64's two
    // branches pass |v| straight into [$ConvertToInt$] (i32) and
    // [$ToBigInt$] (i64), but |v| is not "a JavaScript value" at either call
    // site. |v| is |index| (or |delta|/|descriptor|["initial"/"maximum"])
    // as bound by the calling operation's method steps — per
    // webidl/index.bs:12562-12570 ("create an operation function"), method
    // steps always run with |values| (the result of the *overload
    // resolution algorithm*, i.e. every argument already [=converted to an
    // IDL value=]) as their argument values, never the raw incoming
    // arguments. Every one of these parameters' declared IDL type is `any`
    // (`AddressValue`, webidl/index.bs line 687), and that conversion is
    // real (webidl/index.bs:7314-7339, "convert a JavaScript value to any"):
    // the result is an IDL value (unrestricted double / bigint / DOMString /
    // object / ...) depending on |v|'s ECMAScript type, not the JavaScript
    // value itself. Both `ConvertToInt` (webidl/index.bs:7604, and every
    // real caller — the "convert a JavaScript value to X" family, e.g. line
    // 7523) and `ToBigInt` (ECMA-262) are defined over a genuine JavaScript
    // value, a type webidl/index.bs's own 3.2 ("JavaScript type mapping")
    // explicitly treats as distinct from an IDL value, with its own named
    // conversion in each direction. Rather than patching each call site
    // individually (which would need two different new variable names, one
    // per branch, for what's the same fix applied to the same |v|), hoists a
    // single explicit conversion — `Set |v| to |v|, [=converted to a
    // JavaScript value=].` — right after the algorithm's own head sentence
    // and before either branch, reusing |v|'s own name rather than
    // introducing a fresh one: `Let` (used everywhere else in this file to
    // introduce a genuinely new binding) would be the wrong verb for
    // rebinding a name that's already bound as this algorithm's own
    // parameter — `Set` is the ECMA-262/Bikeshed convention for that. Both
    // branches then read |v| completely unchanged, already carrying the
    // right value. `Set |x| to |x|, [=converted to a JavaScript value=].`
    // parses the same way `Let`'s equivalent does — TextSplit's
    // bracket-depth tracking (`Open`/`Close` on plain `[`/`]`) treats the
    // whole `[=converted to a JavaScript value=]` span as depth > 0, so its
    // own internal "to" (from "converted **to** a JavaScript value") never
    // competes with `SetPrefix`'s outer `" to "` split.
    """converts a JavaScript value to a WebAssembly [=u64=] for use in embedding operations. It is designed to act like [=[EnforceRange]=] [=unsigned long=] for {{AddressType}} "i32", and to extend these semantics to {{AddressType}} "i64", by performing the following steps:

      #1. If |addrtype| is [=i32=],""".stripMargin('#')
    ->
    """converts a JavaScript value to a WebAssembly [=u64=] for use in embedding operations. It is designed to act like [=[EnforceRange]=] [=unsigned long=] for {{AddressType}} "i32", and to extend these semantics to {{AddressType}} "i64", by performing the following steps:

      #1. Set |v| to |v|, [=converted to a JavaScript value=].
      #1. If |addrtype| is [=i32=],""".stripMargin('#'),

    // #40 (spec inconsistency, docs/spec_inconsistencies.md #13) — both spots
    // project a single field out of mem_type/table_type's returned record via
    // "the X in Y(...)", instead of the tuple-destructuring `Let (...) be
    // Y(...)` idiom this same document already uses for table_type at three
    // other call sites (table.get/set/length getter, line 1061/1090/1103).
    // Rewritten to that same destructuring form. table_type's runtime shape
    // (`al_of_tabletype`, construct.ml) is `CaseV("", [addrtype; limits;
    // reftype])` — three real positional fields, matching those three sibling
    // sites' three-element tuple exactly. mem_type's is `CaseV("PAGE",
    // [addrtype; limits])` (`al_of_memorytype`) — only *two* positional
    // fields; `PAGE` there is the record's own tag, not a third field (mirrors
    // memtype's formal grammar, `addrtype limits PAGE`, where `PAGE` is a
    // fixed marker, not a bound component) — hence a two-element tuple, with
    // `page` folded into the ignored second slot's own name (lowercased, to
    // read as an ordinary var name rather than shouting the grammar symbol)
    // so it's still visible in the patched text (`limits page` — both
    // leftover grammar symbols after `addrtype`), even though it binds a
    // single ignored variable, not two.
    "1. Let |addrtype| be the [=address type=] in [=mem_type=](|store|, |memaddr|)."
    ->
    "1. Let (|addrtype|, <var ignore>limits page</var>) be [=mem_type=](|store|, |memaddr|).",
    "1. Let |addrtype| be the [=address type=] in [=table_type=](|store|, |tableaddr|)."
    ->
    "1. Let (|addrtype|, <var ignore>limits</var>, <var ignore>elementtype</var>) be [=table_type=](|store|, |tableaddr|).",

    // #41 retracted (docs/spec_inconsistencies.md #14) — used to rewrite
    // IsFixedLengthArrayBuffer's `[=...=]` calls to `[$...$]`, working around
    // Compiler.nameFromLink lower-casing every `[=...=]` link on the (wrong)
    // assumption that bracket punctuation alone tells local WJI algorithms
    // apart from external mainline AOs. No longer needed: nameFromLink leaves
    // case alone entirely now, and `Interpreter.EClo` retries a failed lookup
    // lowercased, so this resolves correctly regardless of which link syntax
    // the spec text happens to use.

    // #42 (spec inconsistency, docs/spec_inconsistencies.md #15) — "if |op|
    // is a regular operation"/"... a static operation" (2 call sites, both
    // inside "create an operation function") write "regular operation"/
    // "static operation" as plain prose instead of linking them. Just 14
    // lines above the first site, the identical predicate is already written
    // linked ("|op| is not a [=static operation=]", index.bs:12544), and both
    // terms are export'ed dfns (`dfn-regular-operation` line 1883,
    // `dfn-static-operation` line 3002) linked at 15+ other sites throughout
    // this file — these two are the only unlinked occurrences. Linking lets
    // `CondParser.ArticleLink` parse "|op| is a [=regular operation=]" into
    // `Cond.IsType(op, "regular operation")` instead of falling through to
    // `Cond.Unknown`/`EYet("a regular operation")`; `ExpandWjiIsTypePass`
    // resolves that `IsType` against the `kind` field `esmeta.wji.Initialize`
    // already seeds onto every `operation` record. The two sites are indented
    // differently (index.bs:12558-60 sits 12 spaces in, inside a nested
    // sub-step list; 12581-2 sits 4 spaces in, at the outer step list), which
    // shifts Bikeshed's own line-wrapping to a different column at each site
    // and so splits "regular operation" apart at a different point in the
    // source text (after "is a" at 12558-9, mid-word after "regular" at
    // 12581-2) — two byte-distinct strings, so one replacement can't match
    // both. Both reflow the line break so "regular operation" ends up inside
    // one link span instead of split across lines.
    """if |op| is a
      #                regular operation) or for [=static operations=] (if |op| is a static operation)"""
      .stripMargin('#')
    ->
    """if |op| is a
      #                [=regular operation=]) or for [=static operations=] (if |op| is a [=static operation=])"""
      .stripMargin('#'),
    """if |op| is a regular
      #        operation) or for [=static operations=] (if |op| is a static operation)"""
      .stripMargin('#')
    ->
    """if |op| is a
      #        [=regular operation=]) or for [=static operations=] (if |op| is a [=static operation=])"""
      .stripMargin('#'),

    // #43 (spec inconsistency, docs/spec_inconsistencies.md #16) — `read the
    // imports`'s own missing-argument check writes "|importObject| is
    // undefined" instead of "is missing", the idiom every other
    // optional-with-no-default parameter check in this document uses (e.g.
    // `Table`'s constructor/`Table.grow`/`Table.set`'s "|value| is missing",
    // line 1045/1063/1107). Per WebIDL's own overload resolution algorithm
    // (webidl/index.bs: "If optionality is 'optional' and V is undefined
    // ... append ... the special value 'missing'"), a plain `optional object
    // importObject` with no default never actually holds the ES value
    // `undefined` inside an operation's own steps — omitting it (or
    // explicitly passing `undefined`) both convert to the distinct "missing"
    // sentinel before the steps ever run, so "is undefined" checks for a
    // value that, per this same document's own machinery, can't occur here.
    "|importObject| is undefined"
    ->
    "|importObject| is missing",

    // #44 (spec inconsistency, docs/spec_inconsistencies.md #12) — `Memory`/
    // `Table`'s constructors default a missing |descriptor|["address"] to
    // the raw quoted string "i32" instead of the [=i32=] link idiom
    // AddressValueToU64/U64ToAddressValue (patched by #39 above) already
    // expect their own |addrtype| argument to carry — same underlying
    // inconsistency, just at the two call sites that produce |addrtype|
    // rather than the two that consume it. Both `Memory`(line 873) and
    // `Table`(line 1040) write byte-identical text, so one replacement
    // covers both.
    "otherwise, let |addrtype| be \"i32\"."
    ->
    "otherwise, let |addrtype| be [=i32=].",

    // #45 (hardcoding, docs/hardcodes.md #15) — `Memory`'s constructor
    // writes "[=memory type=] |addrtype| { **min** |initial|, **max**
    // |maximum| }" (index.bs:876), Bikeshed's "construct a formal-grammar
    // record value with named fields" notation for a Wasm `memtype` — not a
    // function call. `ExprParser` has no rule for this idiom at all yet
    // (`parseArgs` just tokenizes on whitespace, with no awareness of `{`/
    // `}`/`,`, and `**min**`/`**max**` happen to match `BoldConst`), so it
    // mis-parses the whole thing as a 5-positional-argument call to a
    // nonexistent "memory type" (normalized to `memory_type`) function,
    // crashing with `UnknownFunc` at runtime. `Table`'s constructor
    // (index.bs:1043) has the structurally identical shape but happens to
    // read "**the** [=table type=] ... { ... }" — that leading "the" matches
    // `TypeAnnotatedPrefix` instead, which drops the `[=table type=]` link
    // and parses the remainder as one opaque `Unknown` — inert, not a crash.
    // Prepending "the " to `Memory`'s text routes it through the exact same
    // already-existing (if equally unmechanized) `TypeAnnotatedPrefix` path,
    // trading a misleading "function doesn't exist" crash for this
    // codebase's standard "not yet mechanized" signal (`Expr.Unknown` →
    // `EYet`) — not a real fix (the constructed `memtype` value still isn't
    // usable), just a less misleading failure mode until named-field
    // formal-grammar construction gets properly parsed.
    "be [=memory type=] |addrtype|"
    ->
    "be the [=memory type=] |addrtype|",

    // #46 (spec bug, docs/spec_errors.md #22) — ToWebAssemblyValue's [=host
    // value cache=] hit branch (index.bs:1470) returns immediately, skipping
    // the ref_type/match_valtype check every other branch of the same "ref
    // null heaptype" case funnels through before returning -- so a value
    // already cached under a *broader* type (e.g. passed as anyref) sails
    // through unchecked the next time it's converted for a *narrower* one
    // (e.g. eqref), producing a ref.host value invalid for that type.
    // Rewording just the "Return" to "Let |r| be ..." isn't enough on its
    // own -- the three steps right after it (allocate a fresh hostaddr, cache
    // v under it, bind r) are unconditional top-level steps, so without also
    // gating them behind an "Else," they'd still run unconditionally even on
    // a cache hit, clobbering both the map entry and the just-computed |r|.
    // Wraps them in an "Else," (matching this same algorithm's own "If ...
    // Else if ... Else," idiom a few steps up) so exactly one of the two
    // three-step groups runs, and both join the trailing ref_type/
    // match_valtype/Return |r|. that follows.
    // Matches the text as it stands *after* #31 above already ran (patches
    // apply in list order) -- #31 already dropped this same algorithm's
    // "[=host address=] " annotation from the very `Let |hostaddr| be the
    // smallest address ...` step this patch also touches.
    """1. If a [=host address=] |hostaddr| exists such that |map|[|hostaddr|] is the same as |v|,
      #            1. Return [=ref.host=] |hostaddr|.
      #        1. Let |hostaddr| be the smallest address such that |map|[|hostaddr|] [=map/exists=] is false.
      #        1. [=map/Set=] |map|[|hostaddr|] to |v|.
      #        1. Let |r| be [=ref.host=] |hostaddr|.""".stripMargin('#')
    ->
    """1. If a [=host address=] |hostaddr| exists such that |map|[|hostaddr|] is the same as |v|,
      #            1. Let |r| be [=ref.host=] |hostaddr|.
      #        1. Else,
      #            1. Let |hostaddr| be the smallest address such that |map|[|hostaddr|] [=map/exists=] is false.
      #            1. [=map/Set=] |map|[|hostaddr|] to |v|.
      #            1. Let |r| be [=ref.host=] |hostaddr|.""".stripMargin('#'),

    // #47 (spec bug, docs/spec_errors.md #23) — AddressValueToU64's "i64"
    // branch compares |n| (bound two steps up as [=?=] [$ToBigInt$](|v|), so
    // a BigInt) directly against the plain mathematical-value literals `0`/
    // `2^64 - 1`, instead of first converting it with [=ℝ=] the way every
    // other BigInt-to-mathematical-value use in this document does (e.g. this
    // same step's very next line, "Return [=ℝ=](|n|) as a WebAssembly
    // [=u64=]."). ECMA-262 keeps BigInt and mathematical values as distinct
    // domains that only cross via an explicit conversion notation (𝔽/ℤ/ℝ) —
    // comparing a raw BigInt against a mathematical-value literal isn't
    // itself a defined operation, so this line needs the same [=ℝ=](|n|)
    // conversion the surrounding steps already use, not a special-cased
    // BigInt/mathematical-value comparison rule.
    "If |n| &lt; 0 or |n| &gt; 2<sup>64</sup> &minus; 1, [=throw=] a {{TypeError}}."
    ->
    "If [=ℝ=](|n|) &lt; 0 or [=ℝ=](|n|) &gt; 2<sup>64</sup> &minus; 1, [=throw=] a {{TypeError}}.",

    // #48 (spec inconsistency, docs/spec_inconsistencies.md #17) — the
    // `Memory`/`Table` constructors' `AddressType address;` member is read
    // straight off the JS value (`let |addrtype| be |descriptor|["address"]`),
    // unlike this same document's other `enum`-typed dictionary members —
    // `element`/`value`, read two lines above in these very constructors, both
    // go through [=ToValueType=](|descriptor|["element"/"value"]) instead.
    // `ToValueType`'s own first two branches already map "i32"/"i64" to
    // [=i32=]/[=i64=], the exact case-tag shape `AddressValueToU64`'s `If
    // |addrtype| is [=i32=]` (SpecPatch #39) compares against — so an
    // unconverted |addrtype| never matches either branch there.
    "let |addrtype| be |descriptor|[\"address\"]"
    ->
    "let |addrtype| be [=ToValueType=](|descriptor|[\"address\"])",

    // #49 (spec inconsistency, docs/spec_inconsistencies.md #19) —
    // `attribute setter`'s (webidl/index.bs) own "X is declared with the
    // [{{Y}}] extended attribute" checks for {{Replaceable}},
    // {{LegacyLenientSetter}}, and {{PutForwards}} write "extended attribute"
    // as plain prose, unlike the identical idiom just a few lines above in
    // this very algorithm for {{LegacyLenientThis}} (and the surrounding
    // enumerated-attribute clause covering these same three names,
    // "[{{LegacyLenientSetter}}], [{{PutForwards}}] or [{{Replaceable}}]
    // [=extended attribute=]"), which both link it. `extended attribute` is
    // an export'ed dfn (line 6830); of the 20 "is declared with ... extended
    // attribute" occurrences across this corpus, these 3 are the only
    // unlinked ones. Also normalizes the one non-conditional mention right
    // after ("the identifier argument of the [{{PutForwards}}] extended
    // attribute"), for the same reason, even though it isn't matched by
    // `CondParser` at all (an `ExprParser` context, not a condition) — purely
    // for the algorithm's own internal consistency.
    "If |attribute| is declared with the [{{Replaceable}}] extended attribute, then:"
    ->
    "If |attribute| is declared with the [{{Replaceable}}] [=extended attribute=], then:",
    "If |attribute| is declared with a [{{LegacyLenientSetter}}] extended attribute, then"
    ->
    "If |attribute| is declared with a [{{LegacyLenientSetter}}] [=extended attribute=], then",
    "If |attribute| is declared with a [{{PutForwards}}] extended attribute, then:"
    ->
    "If |attribute| is declared with a [{{PutForwards}}] [=extended attribute=], then:",
    "the identifier argument of the [{{PutForwards}}] extended\n                    attribute."
    ->
    "the identifier argument of the [{{PutForwards}}] [=extended\n                    attribute=].",

    // #50 (spec bug, docs/spec_errors.md #24) — `a [=/new=] {{X}}` (webidl's
    // "new" op — `create a new object implementing the interface`, line
    // 13818) declares a *required* `|realm|` parameter alongside
    // `|interface|`, but every one of the seven call sites in
    // js-api/index.bs omits it outright, leaving nothing for `ExprParser`'s
    // `NewExpr` to bind `|realm|` to (the same shape of defect as #4/#12's
    // `a new promise`/`react` fix — see that pair's own comments above).
    // Each site is corrected to supply `[=current Realm=]`, matching both
    // webidl/index.bs's own established idiom for this exact op elsewhere in
    // that file ("a [=new=] {{DOMException}} created in the [=current
    // realm=]", webidl/index.bs:14896) and this file's own sibling algorithm
    // `a new Exported Function` (line 1259-1276), which already binds `Let
    // |realm| be the current Realm.` at the same call depth five of these
    // seven sites share (reached via `asynchronously instantiate a
    // WebAssembly module` → `create an exports object`, itself run
    // `[=in parallel=]`/`[=Queue a task=]`). Each interface name ({{Module}},
    // {{Instance}}, {{Memory}}, {{Table}}, {{Global}}, {{Tag}}, {{Exception}})
    // occurs in exactly one `a [=/new=] {{X}}` call site in this file, so the
    // bare phrase is a unique, unambiguous anchor for each replacement below.
    "a [=/new=] {{Module}}."
    ->
    "a [=/new=] {{Module}} in the [=current Realm=].",
    "a [=/new=] {{Instance}}."
    ->
    "a [=/new=] {{Instance}} in the [=current Realm=].",
    "a [=/new=] {{Memory}}."
    ->
    "a [=/new=] {{Memory}} in the [=current Realm=].",
    "a [=/new=] {{Table}}."
    ->
    "a [=/new=] {{Table}} in the [=current Realm=].",
    "a [=/new=] {{Global}}."
    ->
    "a [=/new=] {{Global}} in the [=current Realm=].",
    "a [=/new=] {{Tag}}."
    ->
    "a [=/new=] {{Tag}} in the [=current Realm=].",
    "a [=/new=] {{Exception}}."
    ->
    "a [=/new=] {{Exception}} in the [=current Realm=].",

    // #51 (spec bug, docs/spec_errors.md #25) — `inclusive inherited
    // interfaces` (webidl/index.bs:715) advances its walk-up-the-chain loop
    // from the fixed input |I| instead of the loop variable |interface|:
    // "Set |interface| to the [=interface=] that |I| [=interface/inherits=]
    // from, ...". Since |I| never changes, this step always re-derives the
    // same value (|I|'s own immediate parent) on every iteration instead of
    // climbing one level further up each time, so |interface| never reaches
    // null and the loop never terminates for any interface that inherits
    // from anything at all. Patched to advance from |interface| itself.
    "Set |interface| to the [=interface=] that |I| [=interface/inherits=] from, if any, and"
    ->
    "Set |interface| to the [=interface=] that |interface| [=interface/inherits=] from, if any, and",

    // #52 (spec inconsistency, docs/spec_inconsistencies.md #20) — the JSTag
    // getter's prose is written exactly like every other algorithm in this
    // document (a <dfn>, "when invoked, performs the following steps:", a
    // numbered step list) but is missing the <div algorithm> wrapper every
    // other one has -- AlgorithmExtractor.extract only ever looks for that
    // wrapper, so this getter is entirely invisible to it. Two separate
    // insertions (open before the dfn sentence, close after the last step)
    // rather than one big from/to spanning the whole block, per this file's
    // own "shortest text fragment" convention.
    "The getter of the <dfn attribute for=\"WebAssembly\">JSTag</dfn>"
    ->
    "<div algorithm>\nThe getter of the <dfn attribute for=\"WebAssembly\">JSTag</dfn>",
    "1. Return |JSTagObject|."
    ->
    "1. Return |JSTagObject|.\n</div>",

    // #53 (spec bug, docs/spec_errors.md #26) — "a new Exported GC Object"
    // (index.bs:1646-1669) caches purely by |objectaddr|, but Wasm Core's
    // structaddr/arrayaddr are separate address spaces (4.0-execution.
    // configurations.spectec:15-16 -- each just `= addr`, independently
    // allocated from 0 in its own store component), so the first struct and
    // the first array legitimately share objectaddr 0 -- the cache then
    // treats them as the same object (`struct === array`,
    // gc/exported-object.tentative.any.js's "GC objects as map/weak map
    // keys"). Every one of the 3 |map|[|objectaddr|] occurrences in this one
    // algorithm needs the same fix, so one from/to pair (not scoped to a
    // single line) covers the whole thing.
    "|map|[|objectaddr|]"
    ->
    "|map|[(|objectkind|, |objectaddr|)]",

    // #54 (spec bug, docs/spec_errors.md #27) — `define the asynchronous
    // iteration methods` (webidl/index.bs:12981) duplicates the article
    // "an" before "[=asynchronously iterable declaration=]": "does not
    // have an an [=asynchronously iterable declaration=]". Plain wording
    // typo, no semantic effect. Patched to a single "an".
    "does not have an an [=asynchronously iterable declaration=]"
    ->
    "does not have an [=asynchronously iterable declaration=]",

    // #55 (spec bug, docs/spec_errors.md #28) — `get a copy of the bytes
    // held by the buffer source` (webidl/index.bs:9312-9327) reads a view's
    // `[[ByteLength]]` slot directly, but for a *length-tracking* view (one
    // created with no explicit length over a resizable/growable buffer,
    // ECMA-262's TypedArray constructor) that slot is permanently set to the
    // sentinel `~auto~`, not a number -- the real current length has to be
    // recomputed on demand via `MakeTypedArrayWithBufferWitnessRecord` +
    // `TypedArrayByteLength` (ecma262/spec.html:14967-15020), the same way
    // e.g. `%TypedArray%.prototype.length`'s own getter already does.
    // Reading the raw slot instead means this algorithm tries to compare
    // `~auto~` against a number (or use it as one) the moment it's ever
    // handed a length-tracking view -- exactly the shape of buffer this
    // corpus's own "Resizable ArrayBuffer-backed view"/"Growable
    // SharedArrayBuffer-backed view" subtests construct and pass in. Both
    // ops are ordinary mainline ECMA-262 abstract operations already
    // extracted and working (confirmed via the TypedArray length getter),
    // so this patch only needs to route this one step through them --
    // `{{unordered}}`, not a bare `Unordered`/`~unordered~`, to match the
    // WebIDL enum-reference syntax #7 above already normalizes this same
    // file's sibling `[$GetValueFromBuffer$]` call to.
    "        1.  Set |length| to |jsBufferSource|.\\[[ByteLength]]."
    ->
    ("        1.  Let |taRecord| be [$MakeTypedArrayWithBufferWitnessRecord$](|jsBufferSource|, {{unordered}}).\n" +
    "        1.  Set |length| to [$TypedArrayByteLength$](|taRecord|)."),

    // #56 (spec inconsistency, docs/spec_inconsistencies.md #21) —
    // `validate builtins and imported string for a WebAssembly module`
    // (spectec/document/js-api/index.bs:408) constructs the externtype it
    // compares against as one bare backtick-quoted formal-grammar literal,
    // `` `global const (ref extern)` ``, unlike every other externtype
    // construction/destructuring in this corpus (and the sibling `globaltype`
    // construction right next door in `Global`'s own constructor,
    // index.bs:1200: "let |globaltype| be [=const=] |valuetype|"), which all
    // build a value out of linked terms (`[=const=]`/`[=var=]`,
    // `[=external-type/global=]`, `[=ref=]` -- e.g. `ToWebAssemblyValue`,
    // index.bs:1451's already-working `[=ref=] [=heap-type/extern=]`) rather
    // than one opaque literal. Rebuilt from those same three proven-working
    // calls -- but as three separate `Let`s, each binding a plain variable,
    // not two nested calls: `[=external-type/global=] |mut| |reftype|` needs
    // `|mut|`/`|reftype|` as bare already-bound vars for
    // `NormalizeSpecTecCaseShapePass`'s own `[=external-type/global=]`
    // special case (its own doc's worked example --
    // `Case("[=external-type/global=]", [Var(mut), Var(valuetype)])` ->
    // `Case("GLOBAL", [Case("", [Var(mut), Var(valuetype)])])`) to fire on;
    // writing `[=external-type/global=] [=const=] |reftype|` inline instead
    // (a nested call, matching the two-line shape this patch first tried)
    // parses `[=const=] |reftype|` greedily as a single one-arg call before
    // `external-type/global` ever sees it, so its own 2-flat-arg special case
    // never matches and it falls through to a generic one-arg path instead,
    // adding an extra, wrong layer of nesting SpecTec's own `al_to_globaltype`
    // then rejects (confirmed against the real `spectec` binary: `Backend_
    // interpreter.Construct.WrongConversion("globaltype: invalid
    // construction ...")`).
    "        1. Let |stringExternType| be `global const (ref extern)`."
    ->
    ("        1. Let |reftype| be [=ref=] [=heap-type/extern=].\n" +
    "        1. Let |mut| be [=const=].\n" +
    "        1. Let |stringExternType| be [=external-type/global=] |mut| |reftype|."),

    // #57 (spec bug, docs/spec_errors.md #29) — `read the imports`
    // (index.bs:500) is missing `[=?=]` before `[$HasProperty$](|o|,
    // |componentName|)` — every other `[$HasProperty$]`/`[$Get$]` call in
    // this same algorithm (a few lines above/below this one) correctly marks
    // itself `[=?=]`, and `[[HasProperty]]` genuinely can throw (a Proxy
    // trap can run arbitrary code), so this omission isn't a deliberate
    // "provably can't throw here" elision the way a few other bare abstract-
    // op calls elsewhere in this corpus are. Without `[=?=]`, `ExprParser`
    // never wraps this call in `Expr.Abrupt("?", ...)`, so `NormalizeEvaluat
    // ionOrderPass`/`ExpandAbruptPass` never unwrap its `.Value` — the
    // compiled `if (= _call2 false)` then compares the *whole Completion
    // Record* against `false`, which is never `true` regardless of the real
    // boolean, so this branch's "fall back to the plain `importObject`"
    // path can never fire. Confirmed directly: `HasProperty`'s own return
    // value at this call site is a heap `Addr` (a Completion Record), not a
    // bare `Bool`, via a temporary debug print in `Interpreter.eval`'s
    // `ICall`/`IReturn` cases.
    "1. If |o| [=is not an Object=] or if [$HasProperty$](|o|, |componentName|) is false,"
    ->
    "1. If |o| [=is not an Object=] or if [=?=] [$HasProperty$](|o|, |componentName|) is false,",

    // #58 (spec bug, docs/spec_errors.md #31) — `instantiate the core of a
    // WebAssembly module` (index.bs:602) checks `If |result| is [=error=]`
    // before destructuring `|result|` into `(|store|, |instance|)` — but
    // `module_instantiate`'s own declared return type (embedding.rst) is
    // *always* a `(store, moduleinst | error)` pair, so `|result|` itself
    // (the whole tuple) can never literally equal the bare `error` value;
    // only its second component can. Confirmed via `wji-extract`: the
    // condition compiles to a literal `Eq(result, error)`, which is
    // vacuously false forever, so the branch throwing `{{LinkError}}` (etc.)
    // is dead code and a genuine link failure (e.g. a mismatched global's
    // `Externaddr_ok` correctly returning false, confirmed via a temporary
    // debug print in `spectec`'s `Relation.externaddr_ok`/`Embedding.
    // module_instantiate`) falls straight through to the destructuring
    // step, binding `|instance|` to the `error` sentinel value itself and
    // returning it as if it were a real instance — no exception at all.
    // Swaps the destructuring step ahead of the check and tests `|instance|`
    // (the actual second tuple component) instead of `|result|` — same
    // steps, same bulleted exception-type list, just reordered and pointed
    // at the right variable.
    """    1. If |result| is [=error=], throw an appropriate exception type:
        * A {{LinkError}} exception for most cases which occur during linking.
        * If the error came when running the start function, throw a {{RuntimeError}} for most errors which occur from WebAssembly, or the error object propagated from inner ECMAScript code.
        * Another error type if appropriate, for example an out-of-memory exception, as documented in <a href="#errors">the WebAssembly error mapping</a>.
    1. Let (|store|, |instance|) be |result|."""
    ->
    """    1. Let (|store|, |instance|) be |result|.
    1. If |instance| is [=error=], throw an appropriate exception type:
        * A {{LinkError}} exception for most cases which occur during linking.
        * If the error came when running the start function, throw a {{RuntimeError}} for most errors which occur from WebAssembly, or the error object propagated from inner ECMAScript code.
        * Another error type if appropriate, for example an out-of-memory exception, as documented in <a href="#errors">the WebAssembly error mapping</a>.""",

    // #59 (hardcoding, docs/hardcodes.md #22) — collapses the 3-way bulleted
    // exception-type list right after #58's fix into a flat "throw
    // LinkError" — not a spec bug/inconsistency fix, since the bulleted
    // list is itself a perfectly valid (if unusual) description of real
    // WebAssembly embedder behavior; this is a genuine simplification this
    // project can't yet do better than. `spectec`'s own `Embedding.
    // module_instantiate` collapses all three of `$instantiate`'s possible
    // failure modes (a precondition check like `Externaddr_ok` failing
    // *before* any instruction runs; a genuine Wasm trap *during* start-
    // function execution; the start function itself doing a real Wasm
    // `throw`) into one undifferentiated `ERROR` sentinel — its own comment
    // admits "distinguishing them ... is unaddressed; no fixture exercises
    // a throwing start function yet". `func_invoke` already has the
    // machinery for exactly this distinction (`Exception.Trap` -> generic
    // `error`, `Exception.Throw v` -> a separate `EXCEPTION exnaddr`
    // sentinel carrying the real payload) that `module_instantiate` could
    // eventually reuse (`personal/TODO.md`) — but every currently in-scope
    // fixture that reaches this code path is a "bad imports" test, i.e.
    // purely `Exception.Fail` (a precondition failure = linking failure),
    // so "always LinkError" is not just pragmatic but describes 100% of
    // what's actually reachable today; revisit once a fixture with a
    // trapping/throwing start function shows up.
    """    1. If |instance| is [=error=], throw an appropriate exception type:
        * A {{LinkError}} exception for most cases which occur during linking.
        * If the error came when running the start function, throw a {{RuntimeError}} for most errors which occur from WebAssembly, or the error object propagated from inner ECMAScript code.
        * Another error type if appropriate, for example an out-of-memory exception, as documented in <a href="#errors">the WebAssembly error mapping</a>."""
    ->
    "    1. If |instance| is [=error=], throw a {{LinkError}} exception.",

    // #60 (spec bug, docs/spec_errors.md #33) — `fromCharCodeArray` uses its
    // i32-typed |start|/|end| parameters directly in arithmetic without first
    // converting them out of wasm-value form; unlike `docs/spec_errors.md`
    // #33's other two sites, neither is needed in wasm-tagged form again
    // afterward, so both are rebound to their math value in place. No
    // `[=signed_32=]` involved (contrast `ToJSValue`'s own use of it) — that
    // AO exists to produce the JS-*visible* signed reading of an i32, an
    // unrelated concern here; `[=i32.const=]` destructuring alone already
    // yields the raw unsigned payload, and "interpreted as a [=mathematical
    // value=]" just moves that same value into ESMeta's own Math domain with
    // no sign reinterpretation (`Interpreter.scala`'s
    // `(Wasm(NumV(Nat(n))), ToMath) => Math(n)`).
    """1. Let |length| be the number of elements in |array|.
1. If |start| > |end| or |end| > |length|,"""
    ->
    """1. Let [=i32.const=] |start| be |start|.
1. Let |start| be |start| interpreted as a [=mathematical value=].
1. Let [=i32.const=] |end| be |end|.
1. Let |end| be |end| interpreted as a [=mathematical value=].
1. Let |length| be the number of elements in |array|.
1. If |start| > |end| or |end| > |length|,""",

    // #61 (spec bug, docs/spec_errors.md #33) — `intoCharCodeArray`'s
    // i32-typed |start| parameter, same gap: rebound to its math value in
    // place (also never needed in wasm-tagged form again in this algorithm).
    "1. If |start| + |stringLength| > |arrayLength|,"
    ->
    """1. Let [=i32.const=] |start| be |start|.
1. Let |start| be |start| interpreted as a [=mathematical value=].
1. If |start| + |stringLength| > |arrayLength|,""",

    // #62 (spec bug, docs/spec_errors.md #33) — `fromCodePoint`'s i32-typed
    // |v| parameter, same gap, but |v| is still needed in its original
    // wasm-tagged form for the `ToJSValue` call right after — bound to a
    // fresh name instead of rebinding |v| itself.
    "1. If |v| &gt; 0x10ffff,"
    ->
    """1. Let [=i32.const=] |i32| be |v|.
1. Let |i32| be |i32| interpreted as a [=mathematical value=].
1. If |i32| &gt; 0x10ffff,""",
  )

  def apply(source: String): String =
    patches.foldLeft(source) { case (s, (from, to)) => s.replace(from, to) }
