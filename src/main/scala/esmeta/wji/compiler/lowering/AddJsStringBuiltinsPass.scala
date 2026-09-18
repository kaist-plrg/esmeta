package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr, WjiParam}
import esmeta.wji.lang.parser.ExprParser

/** Hand-fills `get_the_builtins_for_a_builtin_set`'s body (js-api/index.bs:
  * 1836: "Return a list of (|name|, |funcType|, |steps|) for the set with name
  * |builtinSetName| defined within this section.") — a self-reference to "this
  * section"'s own thirteen `js-string-*` `<div algorithm>` blocks (already
  * extracted and compiled as ordinary standalone functions, e.g.
  * `js-string-cast`) that no formal algorithm/function call describes, so
  * nothing short of a hardcoded table can fill it in — the same judgment call
  * `find_a_builtin`'s "does not refer to a builtin set" gap already made
  * (`CondParser.RefersToBuiltinSetNeg`/`KnownBuiltinSetNames`).
  *
  * All 13 builtins are covered — 11 whose `funcType` uses only plain
  * value/reftypes (externref/i32/(ref extern)), plus `fromCharCodeArray`/
  * `intoCharCodeArray`, whose `funcType` additionally references a shared Wasm
  * GC array type (`(rec (type (array (mut i16)))).0`,
  * [[foldArrayType]]/[[refNullArrayType]] — `personal/TODO.md` #56, a kind of
  * deftype construction not attempted anywhere else in WJI).
  *
  * For each of the 13, this synthesizes:
  *   - a `funcType` value, built the same way `get_the_javascript_exception_
  *     tag`'s already-working `tag_alloc` call builds one: a raw `Case("[=
  *     comp-type/func=]", [params, results])` (SpecTec's comptype-arrow shape,
  *     `ResolveLinksPass`/`NormalizeSpecTecCaseShapePass`'s normal job to turn
  *     into a real `("->", ...)` value) passed through the `fold` embedding
  *     (`WasmHost.names`), matching `SpecPatch`'s own
  *     `[=fold=]([=comp-type/func=] ...)` correction for that exact call
  *     (`docs/spec_inconsistencies.md`'s `tag_alloc` fix) — `func_alloc` needs
  *     the same registered/folded deftype `tag_alloc` does, just never spelled
  *     out anywhere in prose for js-string's builtins (there's no
  *     `[=tag_alloc=]`-style call site to patch; the funcType is only ever
  *     mentioned as backtick-quoted SpecTec source text above each builtin's
  *     own `<div algorithm>`, never fed through any algorithm step at all).
  *     `ExprParser.parse` on each self-contained param/result's exact link text
  *     (`"[=externref=]"`/`"[=i32=]"`/`"[=ref=] [=heap-type/extern=]"`) reuses
  *     the identical spec vocabulary already proven to resolve correctly
  *     elsewhere in this corpus (`ToValueType`'s `[=i32=]` return,
  *     `SpecPatch`'s own `[=ref=] [=heap-type/extern=]` correction) rather than
  *     hand-encoding the resolved `Case`/`Opt` shapes directly —
  *     [[refNullArrayType]] is the one param with no spec-text link to parse at
  *     all (see its own doc), so it's hand-built the same way.
  *   - a `steps` value: not the raw `js-string-X` algorithm itself (each has a
  *     different arity — 1 to 3 positional params — while `create_a_
  *     builtin_function`'s `hostfunc` needs to invoke whatever `steps` it's
  *     given uniformly, as a single wasm `arguments` list, see
  *     `AddBuiltinFunctionHostfuncPass`), but a small synthetic wrapper
  *     algorithm (`js-string-X-steps`) that destructures `arguments[0..n-1]`
  *     positionally and tail-calls the real one, referenced as a first-class
  *     value via `Expr.AlgoRef` (mirrors "Set X.\[[SLOT]] as specified in
  *     [=ALGO=]"'s own use of the same node for exactly this "value, not a
  *     call" purpose).
  *
  * Runs first in the pipeline (no `requires`) precisely so every pass that
  * would ordinarily process this material when it comes from real spec prose —
  * `ResolveLinksPass`, `NormalizeSpecTecCaseShapePass`,
  * `ExpandPerformReturnResultPass`, `NormalizeAlgoNamePass`, ... — also sees
  * these hand-built algorithms and treats them exactly the same way.
  *
  * Category: Structural desugaring — Injection.
  */
object AddJsStringBuiltinsPass extends LoweringPass:

  private val TargetAlgoName = "get the builtins for a builtin set"

  /** One `js-string-*` builtin: its `<div algorithm="js-string-NAME">`'s own
    * compiled name (already all-lowercase, see that div's own algorithm; not
    * necessarily identical to `name`'s casing), the display `name` used as the
    * builtin-set table's own key (matches `<h4 id="js-string-NAME">`'s exact
    * casing), and its `funcType`'s param/result types -- already-resolved
    * `Expr`s rather than raw link text, since `fromCharCodeArray`/
    * `intoCharCodeArray`'s array-referencing param ([[refNullArrayType]]) isn't
    * spec-linked text `ExprParser.parse` could read at all (see its own doc).
    */
  private case class Builtin(
    algoName: String,
    name: String,
    params: List[Expr],
    results: List[Expr],
  )

  private val Externref = ExprParser.parse("[=externref=]")
  private val I32 = ExprParser.parse("[=i32=]")
  private val RefExtern = ExprParser.parse("[=ref=] [=heap-type/extern=]")

  private val ArrayTypeVar = "_jsStringArrayType"

  /** `(ref null arrayType)` -- `fromCharCodeArray`/`intoCharCodeArray`'s shared
    * Wasm GC array parameter, referencing [[ArrayTypeVar]] once
    * [[foldArrayType]] has folded/registered it. `Case("REF", [nullable-Opt,
    * heaptype])` is `al_to_valtype`'s own required shape (see
    * `NormalizeSpecTecCaseShapePass`'s `ShorthandReftype` doc for the same
    * `Opt(Some(Case("NULL", Nil)))` nullable-flag idiom) -- built directly in
    * already-final runtime-tag form (bypassing `ExprParser`/
    * `NormalizeSpecTecCaseShapePass` entirely, same reasoning as
    * [[foldArrayType]]'s own comptype), since `arrayTypeVar` is a freshly
    * `fold`-bound variable, not anything spec prose ever names.
    */
  private def refNullArrayType(arrayTypeVar: String): Expr =
    Expr.Case(
      "REF",
      List(Expr.Opt(Some(Expr.Case("NULL", Nil))), Expr.Var(arrayTypeVar)),
    )

  private val builtins = List(
    // "Return [=?=] [$UnwrapString$](|v|)" -- returns its own argument
    // unchanged, but that argument is no longer wasm-tagged by the time this
    // algorithm sees it: `create_a_builtin_function_hostfunc`
    // (`AddBuiltinFunctionHostfuncPass`) now converts every argument via
    // `ToJSValue` uniformly before `steps` ever runs, so `cast`'s result needs
    // the same `ToWebAssemblyValue` treatment as any other genuinely-new
    // string, same as the other RefExtern-returning builtins below.
    Builtin("js-string-cast", "cast", List(Externref), List(RefExtern)),
    Builtin("js-string-test", "test", List(Externref), List(I32)),
    Builtin(
      "js-string-fromcharcode",
      "fromCharCode",
      List(I32),
      List(RefExtern),
    ),
    // index.bs:2040 literally says "(result externref)" (nullable) here --
    // every other string-returning builtin in this section (cast/
    // fromCharCode/concat/substring/...) says non-null "(result (ref
    // extern))" instead, and the independently-authored test corpus
    // (spectec/test/js-api/js-string/basic.any.js's own `results:
    // [wasmRefType(kWasmExternRef)]`, non-null) agrees with the *other*
    // builtins' pattern, not with this one's literal text -- treated as a
    // spec typo (`docs/spec_inconsistencies.md`), not transcribed verbatim.
    Builtin(
      "js-string-fromcodepoint",
      "fromCodePoint",
      List(I32),
      List(RefExtern),
    ),
    Builtin(
      "js-string-charcodeat",
      "charCodeAt",
      List(Externref, I32),
      List(I32),
    ),
    Builtin(
      "js-string-codepointat",
      "codePointAt",
      List(Externref, I32),
      List(I32),
    ),
    Builtin("js-string-length", "length", List(Externref), List(I32)),
    Builtin(
      "js-string-concat",
      "concat",
      List(Externref, Externref),
      List(RefExtern),
    ),
    Builtin(
      "js-string-substring",
      "substring",
      List(Externref, I32, I32),
      List(RefExtern),
    ),
    Builtin(
      "js-string-equals",
      "equals",
      List(Externref, Externref),
      List(I32),
    ),
    Builtin(
      "js-string-compare",
      "compare",
      List(Externref, Externref),
      List(I32),
    ),
    // index.bs:1973-1997/1999-2023 -- the only 2 of 13 whose funcType
    // references the shared `(rec (type (array (mut i16)))).0` array type
    // ([[ArrayTypeVar]]/[[foldArrayType]]/[[refNullArrayType]]) rather than a
    // self-contained value/reftype.
    Builtin(
      "js-string-fromcharcodearray",
      "fromCharCodeArray",
      List(refNullArrayType(ArrayTypeVar), I32, I32),
      List(RefExtern),
    ),
    Builtin(
      "js-string-intocharcodearray",
      "intoCharCodeArray",
      List(Externref, refNullArrayType(ArrayTypeVar), I32),
      List(I32),
    ),
  )

  /** the `js-string-NAME-steps` wrapper algorithm: `(arguments) => { return
    * js-string-NAME(arguments[0], ..., arguments[n-1]) }` — uniform 1-arg
    * signature regardless of the real builtin's own arity, so `create_a_
    * builtin_function`'s hostfunc can invoke any `steps` value the same way.
    * `arguments` here is already `create_a_builtin_function_hostfunc`'s own
    * `jsArguments` (`AddBuiltinFunctionHostfuncPass`'s uniform per-argument
    * `ToJSValue` conversion) — every `js-string-NAME` positional parameter this
    * wrapper destructures into is a genuine JS value, never a raw wasm one.
    *
    * Also converts `js-string-NAME`'s own raw result into the `instr*` shape a
    * genuine WebAssembly value must have before crossing back into wasm
    * execution (`docs/spec_errors.md` #32 — `create a builtin function`'s own
    * hostfunc definition never says this needs to happen at all, unlike `create
    * a host function`'s `run a host function`, so this pass has to fill it in):
    *   - `results.head` is `I32` (the 7 numeric builtins, e.g. `js-string-
    *     test`'s "Return 0."): `result` here is a wasm-spec `Math` value (an
    *     arbitrary-precision mathematical integer — every js-string algorithm
    *     body's own arithmetic produces this, the same type e.g. `sizeof`
    *     does), not mainline's ECMAScript `Number` type — confirmed the hard
    *     way: routing it through `ToWebAssemblyValue` first (reusing that one
    *     real conversion mechanism, on the theory that `ToInt32`'s JS coercion
    *     would be a harmless identity transform for an already-clean integer)
    *     crashes instead, `ToNumber`'s own dispatch asserting `argument` must
    *     be `Record[Object]` once every real-Number/Symbol/BigInt/undefined/
    *     null/boolean/String case it checks first has already failed to match
    * -- `Math` and `Number` are genuinely different runtime types here, so
    * `ToWebAssemblyValue`'s JS-value coercion machinery was never the right
    * tool for this case to begin with, harmless-seeming identity transform or
    * not. Wrapped directly as `Case("CONST", [I32, result])` instead -- the
    * same shape `ToWebAssemblyValue`'s own `i32`-branch builds internally, just
    * without the JS-coercion steps that don't apply to an already-wasm-domain
    * value.
    *   - Otherwise (the remaining 5 `RefExtern`-returning builtins, e.g.
    *     `js-string-fromCharCode`'s "Return FromCharCode(v)."): `result` is a
    *     genuinely new or unwrapped JS string -- `ToWebAssemblyValue` is the
    *     only existing mechanism that allocates it a real host address (`[=host
    *     value cache=]`), so it's the right call here. Both cases can
    *     themselves abruptly complete (an abrupt
    *     `js-string-NAME`/`ToWebAssemblyValue` result skips straight to
    *     `Return`ing it, same as `convertedIdlValueBinding`'s own
    *     `AbruptCompletion` check elsewhere).
    */
  private def stepsAlgo(b: Builtin): Algorithm =
    val argumentsVar = Expr.Var("arguments")
    val positional = b.params.indices.map { i =>
      Expr.Index(argumentsVar, Expr.Num(i.toString))
    }.toList
    val listWrap = (e: Expr) => Expr.List_(List(e))
    // `b.results.head` is still raw, unresolved link text at this point
    // (`ExprParser.parse` alone doesn't run `ResolveLinksPass`) -- compared
    // by value against the same `I32` constant every numeric entry in
    // `builtins` is built from, rather than destructured as a `Case`
    // (`Case("I32", Nil)` never matches here, only after the later passes
    // this pass's own class doc says *do* eventually see this same node).
    // A numeric `result` may or may not itself be wrapped in a Completion
    // Record at this point, independent of the abrupt check just above:
    // `CompletionWrapping` only wraps an algorithm's *every* exit (success
    // path included) when it has at least one throw-capable exit of its own
    // (e.g. `equals`/`length`, via `unwrapstring`) -- an algorithm with none
    // at all (e.g. `test`, whose only steps are a plain `? v: String` check)
    // returns its bare value directly. Both shapes reach here, so this
    // branches once more before wrapping.
    def i32Result(e: Expr): Instr =
      Instr.Return(Some(listWrap(Expr.Case("CONST", List(I32, e)))))
    val onNormalResult: List[Instr] =
      if b.results.head == I32 then
        List(
          Instr.IfChain(
            List(
              Cond.IsType(Expr.Var("result"), "Completion") ->
              List(i32Result(Expr.Field(Expr.Var("result"), "Value"))),
            ),
            List(i32Result(Expr.Var("result"))),
          ),
        )
      else
        List(
          Instr.Perform(
            "ToWebAssemblyValue",
            List(Expr.Var("result"), b.results.head),
            Instr.PerformOutcome.BindResult("result"),
          ),
          Instr.IfChain(
            List(
              Cond.IsType(Expr.Var("result"), "AbruptCompletion") ->
              List(Instr.Return(Some(Expr.Var("result")))),
            ),
            List(
              Instr.Return(
                Some(listWrap(Expr.Field(Expr.Var("result"), "Value"))),
              ),
            ),
          ),
        )
    Algorithm(
      id = Some(s"${b.algoName}-steps"),
      name = None,
      params = List(WjiParam("|arguments|")),
      head = "<synthesized by AddJsStringBuiltinsPass>",
      body = List(
        Instr.Perform(
          b.algoName,
          positional,
          Instr.PerformOutcome.BindResult("result"),
        ),
        Instr.IfChain(
          List(
            Cond.IsType(Expr.Var("result"), "AbruptCompletion") ->
            List(Instr.Return(Some(Expr.Var("result")))),
          ),
          onNormalResult,
        ),
      ),
    )

  /** the `Instr.Perform("fold", ..., BindResult(v))` that materializes `b`'s
    * `funcType` into a real registered deftype, bound to a fresh variable.
    */
  private def foldFuncType(b: Builtin, v: String): Instr =
    val funcType = Expr.Case(
      "[=comp-type/func=]",
      List(Expr.List_(b.params), Expr.List_(b.results)),
    )
    Instr.Perform(
      "fold",
      List(funcType),
      Instr.PerformOutcome.BindResult(v),
    )

  /** folds/registers `(array (mut i16))` once, shared by both
    * `fromCharCodeArray`/`intoCharCodeArray`'s [[refNullArrayType]] param --
    * unlike [[foldFuncType]]'s own `Case("[=comp-type/func=]", ...)`, there's
    * no spec-text link for an array comptype at all (its declaration is raw
    * backtick SpecTec grammar, `Let |arrayType| be \`(rec (type (array (mut
    * i16)))).0\`.`, never fed through any algorithm step -- same situation this
    * pass's own class doc already describes for the funcType itself), so this
    * is built directly in `al_to_comptype`/`al_to_fieldtype`'s own final
    * runtime shape (`construct.ml`) rather than going through `ExprParser`:
    * `Case("ARRAY", [fieldtype])`, `fieldtype` itself an untagged `Case("",
    * [mut, storagetype])` pair (mirrors `NormalizeSpecTecCaseShapePass`'s
    * identical untagged-pair idiom for `globaltype`), `mut` an
    * `Opt(Some(Case("MUT", Nil)))` presence flag (`al_ to_mut`: `None` ->
    * immutable, `Some` -> mutable, its *contents* unchecked
    * -- `al_to_globaltype`'s own `[=var=]`/`[=const=]` correction in that same
    * pass uses the identical `Opt(Some(Case("MUT", Nil)))` shape for
    * "mutable"), `storagetype` a packed `Case("I16", Nil)`
    * (`al_to_storagetype`'s dedicated pack-storage case, distinct from a plain
    * valtype).
    */
  private val foldArrayType: Instr =
    val fieldType = Expr.Case(
      "",
      List(Expr.Opt(Some(Expr.Case("MUT", Nil))), Expr.Case("I16", Nil)),
    )
    val arrayComptype = Expr.Case("ARRAY", List(fieldType))
    Instr.Perform(
      "fold",
      List(arrayComptype),
      Instr.PerformOutcome.BindResult(ArrayTypeVar),
    )

  def run(algos: List[Algorithm]): List[Algorithm] =
    val funcTypeVars = builtins.indices.map(i => s"_jsStringFuncType${i + 1}")
    // foldArrayType first -- fromCharCodeArray/intoCharCodeArray's own
    // funcType folds (below) reference ArrayTypeVar, so it must already be
    // bound.
    val foldInstrs =
      foldArrayType +: builtins.zip(funcTypeVars).map(foldFuncType)
    val tableEntries = builtins.zip(funcTypeVars).map { (b, v) =>
      // matches the spec's own "(|name|, |funcType|, |steps|)" notation
      // directly -- `Expr.Tuple` compiles to `ir.ETup`/`Value.Tup`, which
      // (since mainline's `Interpreter`/`ALValueConversion` learned to only
      // `toAL`-convert a `Tup`'s elements lazily, at the point one actually
      // crosses the WasmHost boundary, rather than eagerly at construction)
      // now safely carries `steps` (a closure) same as any other element.
      Expr.Tuple(
        List(
          Expr.Str(b.name),
          Expr.Var(v),
          Expr.AlgoRef(s"${b.algoName}-steps"),
        ),
      )
    }
    val patched = algos.map { a =>
      if a.name.contains(TargetAlgoName) then
        a.copy(body =
          foldInstrs :+ Instr.Return(Some(Expr.List_(tableEntries))),
        )
      else a
    }
    patched ::: builtins.map(stepsAlgo)
