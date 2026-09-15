package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Expr, Instr, WjiParam}
import esmeta.wji.lang.parser.ExprParser

/** Hand-fills `get_the_builtins_for_a_builtin_set`'s body (js-api/index.bs:
  * 1836: "Return a list of (|name|, |funcType|, |steps|) for the set with
  * name |builtinSetName| defined within this section.") — a self-reference
  * to "this section"'s own thirteen `js-string-*` `<div algorithm>` blocks
  * (already extracted and compiled as ordinary standalone functions, e.g.
  * `js-string-cast`) that no formal algorithm/function call describes, so
  * nothing short of a hardcoded table can fill it in — the same judgment call
  * `find_a_builtin`'s "does not refer to a builtin set" gap already made
  * (`CondParser.RefersToBuiltinSetNeg`/`KnownBuiltinSetNames`).
  *
  * Scoped to the 11 builtins whose `funcType` uses only plain value/reftypes
  * (externref/i32/(ref extern)) — `fromCharCodeArray`/`intoCharCodeArray`
  * additionally need a shared Wasm GC array type (`(rec (type (array (mut
  * i16)))).0`), a kind of deftype construction not yet attempted anywhere in
  * WJI, and are deliberately left out for now (still `Unknown`/absent from
  * the table, so any test path reaching them fails loudly rather than
  * silently misbehaving).
  *
  * For each of the 11, this synthesizes:
  *   - a `funcType` value, built the same way `get_the_javascript_exception_
  *     tag`'s already-working `tag_alloc` call builds one: a raw `Case("[=
  *     comp-type/func=]", [params, results])` (SpecTec's comptype-arrow
  *     shape, `ResolveLinksPass`/`NormalizeSpecTecCaseShapePass`'s normal
  *     job to turn into a real `("->", ...)` value) passed through the
  *     `fold` embedding (`WasmHost.names`), matching `SpecPatch`'s own
  *     `[=fold=]([=comp-type/func=] ...)` correction for that exact call
  *     (`docs/spec_inconsistencies.md`'s `tag_alloc` fix) — `func_alloc`
  *     needs the same registered/folded deftype `tag_alloc` does, just never
  *     spelled out anywhere in prose for js-string's builtins (there's no
  *     `[=tag_alloc=]`-style call site to patch; the funcType is only ever
  *     mentioned as backtick-quoted SpecTec source text above each builtin's
  *     own `<div algorithm>`, never fed through any algorithm step at all).
  *     `ExprParser.parse` on each param/result's exact link text (`"[=
  *     externref=]"`/`"[=i32=]"`/`"[=ref=] [=heap-type/extern=]"`) reuses the
  *     identical spec vocabulary already proven to resolve correctly
  *     elsewhere in this corpus (`ToValueType`'s `[=i32=]` return,
  *     `SpecPatch`'s own `[=ref=] [=heap-type/extern=]` correction) rather
  *     than hand-encoding the resolved `Case`/`Opt` shapes directly.
  *   - a `steps` value: not the raw `js-string-X` algorithm itself (each has
  *     a different arity — 1 to 3 positional params — while `create_a_
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
  * would ordinarily process this material when it comes from real spec
  * prose — `ResolveLinksPass`, `NormalizeSpecTecCaseShapePass`,
  * `ExpandPerformReturnResultPass`, `NormalizeAlgoNamePass`, ... — also sees
  * these hand-built algorithms and treats them exactly the same way.
  *
  * Category: Structural desugaring — Injection.
  */
object AddJsStringBuiltinsPass extends LoweringPass:

  private val TargetAlgoName = "get the builtins for a builtin set"

  /** One `js-string-*` builtin: its `<div algorithm="js-string-NAME">`'s own
    * compiled name (already all-lowercase, see that div's own algorithm; not
    * necessarily identical to `name`'s casing), the display `name` used as
    * the builtin-set table's own key (matches `<h4 id="js-string-NAME">`'s
    * exact casing), and its `funcType`'s param/result link texts.
    */
  private case class Builtin(
    algoName: String,
    name: String,
    params: List[String],
    results: List[String],
  )

  private val Externref = "[=externref=]"
  private val I32 = "[=i32=]"
  private val RefExtern = "[=ref=] [=heap-type/extern=]"

  private val builtins = List(
    Builtin("js-string-cast", "cast", List(Externref), List(RefExtern)),
    Builtin("js-string-test", "test", List(Externref), List(I32)),
    Builtin(
      "js-string-fromcharcode",
      "fromCharCode",
      List(I32),
      List(RefExtern),
    ),
    Builtin(
      "js-string-fromcodepoint",
      "fromCodePoint",
      List(I32),
      List(Externref),
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
  )

  /** the `js-string-NAME-steps` wrapper algorithm: `(arguments) => { return
    * js-string-NAME(arguments[0], ..., arguments[n-1]) }` — uniform 1-arg
    * signature regardless of the real builtin's own arity, so `create_a_
    * builtin_function`'s hostfunc can invoke any `steps` value the same way.
    */
  private def stepsAlgo(b: Builtin): Algorithm =
    val argumentsVar = Expr.Var("arguments")
    val positional = b.params.indices.map { i =>
      Expr.Index(argumentsVar, Expr.Num(i.toString))
    }.toList
    Algorithm(
      id = Some(s"${b.algoName}-steps"),
      name = None,
      params = List(WjiParam("|arguments|")),
      head = "<synthesized by AddJsStringBuiltinsPass>",
      body = List(
        Instr.Perform(
          b.algoName,
          positional,
          Instr.PerformOutcome.ReturnResult,
        ),
      ),
    )

  /** the `Instr.Perform("fold", ..., BindResult(v))` that materializes `b`'s
    * `funcType` into a real registered deftype, bound to a fresh variable.
    */
  private def foldFuncType(b: Builtin, v: String): Instr =
    val funcType = Expr.Case(
      "[=comp-type/func=]",
      List(
        Expr.List_(b.params.map(ExprParser.parse)),
        Expr.List_(b.results.map(ExprParser.parse)),
      ),
    )
    Instr.Perform(
      "fold",
      List(funcType),
      Instr.PerformOutcome.BindResult(v),
    )

  def run(algos: List[Algorithm]): List[Algorithm] =
    val funcTypeVars = builtins.indices.map(i => s"_jsStringFuncType${i + 1}")
    val foldInstrs = builtins.zip(funcTypeVars).map(foldFuncType)
    val tableEntries = builtins.zip(funcTypeVars).map { (b, v) =>
      // a plain heap `List_`, not `Expr.Tuple` -- `Compiler.compileExpr`
      // compiles `Tuple` straight to `ir.ETup`, whose own `eval` (mainline
      // `Interpreter.scala`) unconditionally `toAL`-converts every element
      // (it's the Wasm-*value* tuple representation, e.g. a `func_alloc`
      // result's `(store, funcaddr)`) -- `steps` (a closure) can't cross that
      // boundary and was never meant to (`func_alloc`'s own Scala-side
      // `toHostFunc` accepts a closure value directly, no ALValue needed).
      Expr.List_(
        List(Expr.Str(b.name), Expr.Var(v), Expr.AlgoRef(s"${b.algoName}-steps")),
      )
    }
    val patched = algos.map { a =>
      if a.name.contains(TargetAlgoName) then
        a.copy(body = foldInstrs :+ Instr.Return(Some(Expr.List_(tableEntries))))
      else a
    }
    patched ::: builtins.map(stepsAlgo)
