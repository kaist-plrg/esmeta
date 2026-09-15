package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr, WjiParam}

/** Hand-fills `create_a_builtin_function`'s `hostfunc` binding (js-api/
  * index.bs:1872: "Let |hostfunc| be a [=host function=] which executes
  * |steps| when called.") — parsed as `Instr.Let(Var("hostfunc"), Unknown(
  * ...), ...)` since "a [=host function=] which executes |steps| when
  * called" doesn't match any of `ExprParser`'s closure-literal shapes
  * (`WhichPerformsStepsClosure` etc. all expect the closure's own steps
  * spelled out inline as further numbered sub-steps — `|steps|` here is
  * instead a value, already bound as this algorithm's own second parameter).
  *
  * Synthesizes a real closure the same way `create_a_host_function`'s own
  * (already-working) `hostfunc` does — `Expr.Closure(name, captured)`,
  * `func_alloc`'s existing generic Scala-side wiring (mainline
  * `Interpreter.toHostFunc`, which accepts *any* closure value regardless of
  * where it came from) needs no changes at all — except calling `steps`
  * itself as a runtime closure *value* (`Instr.PerformClosure`, not
  * `Instr.Perform`, since unlike `create_a_host_function_closure1`'s `run_a_
  * host_function(func, functype, arguments)` there's no single fixed
  * algorithm to name here: `steps` is whichever `js-string-X-steps` wrapper
  * `AddJsStringBuiltinsPass`'s table happened to hand this particular
  * `funcaddr` at `instantiate_a_builtin_set` time), and treating every
  * abrupt result as a genuine Wasm trap unconditionally, rather than
  * `create_a_host_function_closure1`'s `(ref.exn) throw_ref` JS-exception
  * passthrough — correct for every js-string builtin currently in the table
  * (`AddJsStringBuiltinsPass`'s scoped 11): each one's only possible throw is
  * `ExprParser.TrapException`'s `Case("TRAP", Nil)` (`docs/hardcodes.md`; "a
  * {{RuntimeError}} exception as if a [=trap=] was executed", 10
  * occurrences, always this exact phrasing) — never a genuine JS value/
  * `Exception` object needing to cross the Wasm boundary as a catchable
  * `throw_ref`. Revisit (branch on the abrupt value's shape, building the
  * `(ref.exn) throw_ref` pair for anything that isn't `Case("TRAP", Nil)`)
  * if a future builtin set's `steps` can throw something else.
  *
  * Runs alongside [[AddJsStringBuiltinsPass]] (see its own doc — same
  * "very early in the pipeline" placement, same reasoning), but as a
  * separate pass since it targets a different algorithm and neither needs
  * anything the other produces.
  *
  * Category: Structural desugaring — Injection.
  */
object AddBuiltinFunctionHostfuncPass extends LoweringPass:

  private val TargetAlgoName = "create a builtin function"
  private val HostfuncAlgoId = "create_a_builtin_function_hostfunc"

  private val agentStore =
    Expr.Field(Expr.SpecTerm("surrounding agent"), "associated store")

  private def hostfuncAlgo: Algorithm =
    Algorithm(
      id = Some(HostfuncAlgoId),
      name = None,
      params = List(WjiParam("|state|"), WjiParam("|arguments|")),
      head = "<synthesized by AddBuiltinFunctionHostfuncPass>",
      body = List(
        Instr.Set(agentStore, Expr.Var("state")),
        Instr.PerformClosure(
          Expr.Var("steps"),
          List(Expr.Var("arguments")),
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

  def run(algos: List[Algorithm]): List[Algorithm] =
    val patched = algos.map { a =>
      if a.name.contains(TargetAlgoName) then a.copy(body = patchHostfuncLet(a.body))
      else a
    }
    patched :+ hostfuncAlgo
