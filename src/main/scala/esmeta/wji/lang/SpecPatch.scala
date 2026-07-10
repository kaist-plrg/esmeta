package esmeta.wji.lang

/** Known corrections for spec errors in `spectec/document/js-api/index.bs`.
  *
  * Each entry is a `(from, to)` pair applied as a literal string replacement on
  * the raw source text before parsing. Entries correspond 1-to-1 with the
  * errors documented in `mechanize/spec_errors.md`.
  */
object SpecPatch:

  /** All patches in application order. */
  val patches: List[(String, String)] = List(
    // #1 — empty ordered map literal written as « » instead of «[ ]»
    "Let |builtinOrStringImports| be the ordered map « »."
    -> "Let |builtinOrStringImports| be the ordered map «[ ]».",

    // #2 — variable `keys` missing pipe delimiters in [[OwnPropertyKeys]]
    "1. Let keys be a new empty list.\n    1. Return keys."
    -> "1. Let |keys| be a new empty list.\n    1. Return |keys|.",

    // #3 - (TODO: This is temporary, and shoulb de fixed) Rename instantiate function to avoid overlaod
    "The <dfn method for=\"WebAssembly\">instantiate(|moduleObject|, |importObject|)</dfn> method, when invoked, performs the following steps:"
    ->
    "The <dfn method for=\"WebAssembly\">instantiate_object(|moduleObject|, |importObject|)</dfn> method, when invoked, performs the following steps:",

    // #4 — `a new promise` is defined with an explicit |realm| parameter
    // (webidl/index.bs), but js-api/index.bs calls it with no argument,
    // eliding the realm the way Bikeshed prose conventionally does. Make the
    // realm argument explicit so it compiles.
    "1. Let |promise| be [=a new promise=]."
    -> "1. Let |promise| be [=a new promise=] in the [=current Realm=].",

    // #5 — `|module|.[=imports=]` treats a decoded `module` as a record with
    // named fields, a leftover from an older version of the Wasm Core Spec.
    // In the current (post-GC-proposal) spec/SpecTec representation, `module`
    // is an opaque positional value with no named fields — the only correct
    // way to read its imports is through the `module_imports` embedding
    // function, exactly as every other use site in this file already does
    // (e.g. lines 405, 474, 497, 736).
    "1. If |module|.[=imports=] [=list/is empty|is not empty=], and |importObject| is undefined, throw a {{TypeError}} exception."
    ->
    "1. If [=module_imports=](|module|) [=list/is empty|is not empty=], and |importObject| is undefined, throw a {{TypeError}} exception.",
    """1.  For |i| in [=the range=] |offset| to |offset| + |length| &minus; 1, inclusive, set
        |bytes|[|i| &minus; |offset|] to [$GetValueFromBuffer$](|jsArrayBuffer|, |i|, Uint8,
        true, Unordered)."""
    ->
    "1.  For |i| in [=the range=] |offset| to |offset| + |length| &minus; 1, inclusive, set |bytes|[|i| &minus; |offset|] to [$GetValueFromBuffer$](|jsArrayBuffer|, |i|, {{uint8}}, true, {{unordered}})."

  )

  def apply(source: String): String =
    patches.foldLeft(source) { case (s, (from, to)) => s.replace(from, to) }
