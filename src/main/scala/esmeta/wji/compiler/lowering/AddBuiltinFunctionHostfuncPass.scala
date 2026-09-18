package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr, WjiParam}

/** Hand-fills `create_a_builtin_function`'s `hostfunc` binding (js-api/
  * index.bs:1872: "Let |hostfunc| be a [=host function=] which executes
  * |steps| when called.") — parsed as `Instr.Let(Var("hostfunc"), Unknown(
  * ...), ...)` since "a [=host function=] which executes |steps| when called"
  * doesn't match any of `ExprParser`'s closure-literal shapes
  * (`WhichPerformsStepsClosure` etc. all expect the closure's own steps spelled
  * out inline as further numbered sub-steps — `|steps|` here is instead a
  * value, already bound as this algorithm's own second parameter).
  *
  * Synthesizes a real closure the same way `create_a_host_function`'s own
  * (already-working) `hostfunc` does — `Expr.Closure(name, captured)`,
  * `func_alloc`'s existing generic Scala-side wiring (mainline
  * `Interpreter.toHostFunc`, which accepts *any* closure value regardless of
  * where it came from) needs no changes at all — except calling `steps` itself
  * as a runtime closure *value* (`Instr.PerformClosure`, not `Instr.Perform`,
  * since unlike `create_a_host_function_closure1`'s `run_a_ host_function(func,
  * functype, arguments)` there's no single fixed algorithm to name here:
  * `steps` is whichever `js-string-X-steps` wrapper `AddJsStringBuiltinsPass`'s
  * table happened to hand this particular `funcaddr` at
  * `instantiate_a_builtin_set` time), and treating every abrupt result as a
  * genuine Wasm trap unconditionally, rather than
  * `create_a_host_function_closure1`'s `(ref.exn) throw_ref` JS-exception
  * passthrough — correct for every js-string builtin currently in the table
  * (`AddJsStringBuiltinsPass`'s scoped 11): each one's only possible throw is
  * `ExprParser.TrapException`'s `Case("TRAP", Nil)` (`docs/hardcodes.md`; "a
  * {{RuntimeError}} exception as if a [=trap=] was executed", 10 occurrences,
  * always this exact phrasing) — never a genuine JS value/ `Exception` object
  * needing to cross the Wasm boundary as a catchable `throw_ref`. Revisit
  * (branch on the abrupt value's shape, building the `(ref.exn) throw_ref` pair
  * for anything that isn't `Case("TRAP", Nil)`) if a future builtin set's
  * `steps` can throw something else.
  *
  * Also registers every `funcaddr` `create_a_builtin_function` mints into
  * `@AGENT_RECORD["function import list"]` — a second, unrelated gap this
  * algorithm has, found only once `match_externtype` (`docs/hardcodes.md`
  * #21/`personal/TODO.md` #54, a spec-inconsistent `fromCodePoint` funcType
  * nullability — since fixed) stopped blocking `new WebAssembly.Instance(...)`
  * from actually running `instantiate_a_builtin_set`. `name_of_the_
  * WebAssembly_function` (index.bs:1245, called when `.exports.foo` first
  * creates an Exported Function for a builtin) checks whether `funcinst.code`
  * is a `HOSTFUNC` and, if so, looks its `funcaddr` up by linear search in this
  * exact list — a WJI-hardcoded stand-in for "index of the host function"
  * (index.bs:507's own scoped-to-`read_the_imports` definition,
  * `docs/hardcodes.md`'s existing mechanization elsewhere). `read_the_imports`
  * already pushes onto it for every `create_a_host_function`-made `funcaddr`
  * (its own `push @AGENT_RECORD["function import list"] < funcaddr` right after
  * that call) — but `create_a_builtin_function` mints its own `funcaddr` via a
  * completely different call path (`instantiate_a_builtin_set`, which runs
  * *before* `read_the_imports`'s own per-import loop even starts) that never
  * touched this list at all, so every js-string builtin's `funcaddr` was
  * invisible to it — `assertion failure: (contains funcaddrs funcaddr)` the
  * moment JS code first reads `instance.exports.cast` (etc.).
  *
  * Runs alongside [[AddJsStringBuiltinsPass]] (see its own doc — same "very
  * early in the pipeline" placement, same reasoning), but as a separate pass
  * since it targets a different algorithm and neither needs anything the other
  * produces.
  *
  * Category: Structural desugaring — Injection.
  */
object AddBuiltinFunctionHostfuncPass extends LoweringPass:

  private val TargetAlgoName = "create a builtin function"
  private val HostfuncAlgoId = "create_a_builtin_function_hostfunc"

  private val agentStore =
    Expr.Field(Expr.SpecTerm("surrounding agent"), "associated store")
  private val agentFunctionImportList =
    Expr.Field(Expr.SpecTerm("surrounding agent"), "function import list")

  /** `steps`, unlike `run a host function`'s callee, is never a genuine JS
    * function — it's always one of `AddJsStringBuiltinsPass`'s own `js-string-
    * X-steps` wrappers, called via `Instr.PerformClosure` rather than a plain
    * `Call`. But per `docs/spec_errors.md` #32, the real spec text never says
    * what domain `steps`'s own arguments/result should be in either, and the
    * most consistent answer — mirroring `run a host function`'s own "For each
    * arg of arguments, Append [=ToJSValue=](arg) to jsArguments" step
    * (index.bs:1326-1327) exactly, uniformly over every argument regardless of
    * its declared wasm type — is to hand `steps` genuine JS values throughout,
    * not raw wasm ones. This is why the js-string builtins' own bodies (e.g.
    * `FromCharCode`'s "Assert: v is of type i32") don't call `ToJSValue`
    * themselves: they already receive an already-converted JS value.
    */
  private def buildJsArguments: List[Instr] =
    List(
      Instr.Let(Expr.Var("jsArguments"), Expr.List_(Nil)),
      Instr.Let(Expr.Var("_i"), Expr.Num("0")),
      Instr.While(
        Cond.Compare(
          Expr.Var("_i"),
          Cond.CompareOp.Lt,
          Expr.Length(Expr.Var("arguments")),
        ),
        List(
          Instr.Let(
            Expr.Var("arg"),
            Expr.Index(Expr.Var("arguments"), Expr.Var("_i")),
          ),
          Instr.Perform(
            "ToJSValue",
            List(Expr.Var("arg")),
            Instr.PerformOutcome.BindResult("jsArg"),
          ),
          Instr.Append(Expr.Var("jsArg"), Expr.Var("jsArguments")),
          Instr.Set(
            Expr.Var("_i"),
            Expr.BinOp(Expr.Var("_i"), Expr.BOp.Add, Expr.Num("1")),
          ),
        ),
      ),
    )

  private def hostfuncAlgo: Algorithm =
    Algorithm(
      id = Some(HostfuncAlgoId),
      name = None,
      params = List(WjiParam("|state|"), WjiParam("|arguments|")),
      head = "<synthesized by AddBuiltinFunctionHostfuncPass>",
      body = buildJsArguments ++ List(
        Instr.Set(agentStore, Expr.Var("state")),
        Instr.PerformClosure(
          Expr.Var("steps"),
          List(Expr.Var("jsArguments")),
          Instr.PerformOutcome.BindResult("result"),
        ),
        Instr.Let(Expr.Var("store"), agentStore),
        Instr.IfChain(
          List(
            (
              Cond.IsType(Expr.Var("result"), "AbruptCompletion"),
              List(
                Instr.Return(
                  Some(
                    Expr.Tuple(
                      List(
                        Expr.Var("store"),
                        Expr.List_(List(Expr.Case("TRAP", Nil))),
                      ),
                    ),
                  ),
                ),
              ),
            ),
          ),
          List(
            Instr.Return(
              Some(Expr.Tuple(List(Expr.Var("store"), Expr.Var("result")))),
            ),
          ),
        ),
      ),
    )

  private def patchHostfuncLet(instrs: List[Instr]): List[Instr] =
    instrs.map {
      case l @ Instr.Let(Expr.Var("hostfunc"), _, body) =>
        l.copy(
          expr = Expr.Closure(HostfuncAlgoId, List("steps")),
          body = patchHostfuncLet(body),
        )
      case i => i.mapBody(patchHostfuncLet)
    }

  /** inserts `push @AGENT_RECORD["function import list"] < funcaddr` right
    * before every `Return funcaddr` in `create_a_builtin_function`'s body — see
    * this object's own doc for why.
    */
  private def registerFuncaddr(instrs: List[Instr]): List[Instr] =
    instrs.flatMap {
      case r @ Instr.Return(Some(Expr.Var("funcaddr")), body) =>
        List(
          Instr.Append(Expr.Var("funcaddr"), agentFunctionImportList),
          r.copy(body = registerFuncaddr(body)),
        )
      case i => List(i.mapBody(registerFuncaddr))
    }

  def run(algos: List[Algorithm]): List[Algorithm] =
    val patched = algos.map { a =>
      if a.name.contains(TargetAlgoName) then
        a.copy(body = registerFuncaddr(patchHostfuncLet(a.body)))
      else a
    }
    patched :+ hostfuncAlgo
