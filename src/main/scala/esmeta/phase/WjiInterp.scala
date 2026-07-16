package esmeta.phase

import esmeta.*
import esmeta.util.*
import esmeta.cfg.CFG
import esmeta.cfgBuilder.CFGBuilder
import esmeta.error.ESMetaError
import esmeta.es.Initialize
import esmeta.es.builtin.{AGENT_RECORD, EXECUTION_STACK, realmAddr}
import esmeta.interpreter.{Interpreter => EsInterpreter}
import esmeta.ir.{Global, Local, Program}
import esmeta.state.*
import esmeta.wji.bridge.SpecTecWasmHost
import esmeta.wji.bridge.process.SpecTecProcess
import esmeta.wji.bridge.rpc.JsonRpcConnection
import esmeta.wji.compiler.Compiler
import esmeta.wji.compiler.lowering.Lowering
import esmeta.wji.lang.SpecFile
import scala.collection.mutable.{Map => MMap}

/** `wji-interp` phase
  *
  * A debugging tool: invokes a single compiled WJI function directly (by name,
  * with synthetic args), instead of requiring a full JS driver file like
  * `wji-eval`. Algorithms that touch `the current Realm Record` (or any other
  * execution-context-scoped state) need a live realm to run against, and — per
  * ECMA-262 9.4 — a realm/execution context only really exists while a script
  * is actively being evaluated; `esmeta.es.Initialize` alone only builds the
  * heap skeleton (empty execution context stack, an empty Realm Record shell),
  * not the wiring (Realm.Intrinsics, the pushed execution context). That wiring
  * happens only by actually running ECMA-262's own mechanized bootstrap
  * (`InitializeHostDefinedRealm` / `ScriptEvaluation`), and per spec the
  * execution context stack is popped back to empty once that job finishes.
  *
  * So this phase runs a small bootstrap script to completion first (see
  * `apply`) — which pushes a context, wires up the realm/intrinsics, then pops
  * the context again — and then, reusing that same (heap-populated) [[State]],
  * manually re-pushes a fresh execution context referencing the now-fully-wired
  * realm before jumping directly into the target WJI function. This is a
  * stand-in for "a minimal JS driver called `entry`".
  *
  * Wasm embedding calls (`ICallEmbed`) are dispatched to a live
  * [[SpecTecWasmHost]] over JSON-RPC, so this requires a live SpecTec process
  * on `PATH` (see [[SpecTecProcess]]).
  */
case object WjiInterp extends Phase[CFG, Value] {
  val name = "wji-interp"
  val help =
    "invokes a single compiled WJI function against a live ECMA-262 realm (needs SpecTec)."

  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Value =
    val algorithms = SpecFile.loadAllAlgorithms()
    val wjiProgram = Compiler.compile(Lowering.run(algorithms))
    val merged =
      Program(cfg.program.funcs ++ wjiProgram.funcs, cfg.program.spec)
    val mergedCfg = CFGBuilder(merged)
    given CFG = mergedCfg

    // run a script to completion so the real ECMA-262 bootstrap wires up the
    // realm/intrinsics; the execution context it pushed is popped again by
    // the time this returns, but the Realm Record it populated lives on in
    // the heap. It also builds `importObj`, the {js: {import1, import2}}
    // object js-api/index.bs's own sample usage (§Sample API Usage,
    // index.bs:288-318) passes to instantiation — real ECMAScript, run by the
    // real ECMA-262 mechanization, rather than a hand-built heap record, so
    // it round-trips through `[$Get$]`/`[$IsCallable$]` exactly like a real
    // import object would. `import1`/`import2` are no-ops here (the sample's
    // own bodies call `console.log`, which isn't a real ECMA-262 global) —
    // this is only meant to exercise `create a host function`, not their
    // console output.
    val st = EsInterpreter(
      Initialize(mergedCfg).from("""
        var importObj = {js: {
          import1: function() {},
          import2: function() {}
        }};
      """),
    )

    // re-push a fresh execution context referencing that now-populated realm
    // so `the current Realm Record` (and friends) resolve for the calls below
    // (`RunJobs` — manuals/algos/RunJobs.algo — pops the context stack back to
    // empty once the script job finishes).
    val execContext = st.heap.allocRecord(
      "ExecutionContext",
      List("Realm" -> realmAddr, "Function" -> Null, "ScriptOrModule" -> Null),
    )
    st.heap.push(NamedAddr(EXECUTION_STACK), execContext, true)

    // `var importObj = ...` makes it a property of the real global object —
    // NOT the script's completion value: `RunJobs`'s script-evaluation job
    // unconditionally `Return *undefined*.`s, discarding whatever the script
    // itself evaluated to, so `GLOBAL_RESULT` was never set to it. Read it
    // back out with two real AO calls instead — `GetGlobalObject()` (the
    // current Realm's `[[GlobalObject]]`, not the bare `NamedAddr(GLOBAL)`
    // constant, which is only `Initialize`'s pre-bootstrap placeholder and
    // stays a near-empty record even after the script runs), then
    // `Get(globalObj, "importObj")` — the same "set up a call, run, read
    // GLOBAL_RESULT" shape as the target-function invocation below.
    //
    // Both are compiled with `needRetComp` (any AO that can appear behind a
    // `?`/`!` in spec prose has every `Return` wrapped — see
    // `esmeta.compiler.Compiler`), so `GLOBAL_RESULT` here is a
    // `NormalCompletion` *record*, not the raw value: unwrap it before using
    // it as an argument to anything else, or a later real `[[Get]]` internal
    // method call ends up receiving the completion record itself as its
    // receiver and fails looking for a "Get" field on it.
    def unwrapCompletion(v: Value): Value = v match
      case addr: Addr =>
        st.heap(addr) match
          case r: RecordObj if r.tname == "CompletionRecord" =>
            r.get("Type") match
              case Some(Enum("normal")) => r.get("Value").getOrElse(Undef)
              case _ =>
                throw new RuntimeException(s"abrupt completion: ${st(addr)}")
          case _ => v
      case _ => v
    def callAO(name: String, args: List[Value]): Value =
      val f = mergedCfg.getFunc(name)
      st.context = Context(f, MMap.from(f.params.map(_.lhs).zip(args)))
      st.callStack = Nil
      EsInterpreter(st)
      unwrapCompletion(st.globals.getOrElse(GLOBAL_RESULT, Undef))
    val globalObjAddr = callAO("GetGlobalObject", Nil)
    val importObj = callAO("Get", List(globalObjAddr, Str("importObj")))

    val func = mergedCfg.getFunc(config.entry)

    val process = SpecTecProcess.start()
    val connection = JsonRpcConnection.stdio(process)
    val host = SpecTecWasmHost(connection)

    // "Each agent has an associated store. When a new agent is created, its
    // associated store is set to the result of store_init()." (index.bs:334)
    // — plain prose, not a `<div algorithm>`, so it's never mechanized as a
    // step; the harness seeds it directly here, the same way it manually
    // re-wires the execution context above. See Compiler's
    // `SpecTerm("surrounding agent")` case for how spec references to it
    // (`the surrounding agent's associated store`, etc.) resolve to this slot.
    host.call("store_init", Nil) match
      case Right(store) =>
        st.heap.update(
          NamedAddr(AGENT_RECORD),
          Str("associated store"),
          Wasm(store),
        )
      case Left(err) =>
        throw new RuntimeException(s"store_init failed: $err")

    // js-api/index.bs's own sample usage (§Sample API Usage, index.bs:288-301,
    // `demo.wat`): two func imports ("js"."import1"/"import2"), a start
    // function calling the first, and an export "f" calling the second —
    // compiled with `wat2wasm` and inlined as a byte literal. Decoded through
    // the real `module_decode` embedding call (not a hand-built placeholder
    // `CaseV("MODULE", ...)`), so it round-trips through `module_imports`,
    // `module_instantiate`, etc. exactly like a real module would.
    val demoWasmBytes: List[Int] = List(
      0, 97, 115, 109, 1, 0, 0, 0, 1, 4, 1, 96, 0, 0, 2, 27, 2, 2, 106, 115, 7,
      105, 109, 112, 111, 114, 116, 49, 0, 0, 2, 106, 115, 7, 105, 109, 112,
      111, 114, 116, 50, 0, 0, 3, 3, 2, 0, 0, 7, 5, 1, 1, 102, 0, 3, 8, 1, 2,
      10, 11, 2, 4, 0, 16, 0, 11, 4, 0, 16, 1, 11,
    )
    val demoWasmBytesAL: ALValue =
      ALValue.ListV(demoWasmBytes.map(n => ALValue.NumV(ALNum.Nat(n))))
    val decodedModule = host.call("module_decode", List(demoWasmBytesAL)) match
      case Right(m) => Wasm(m)
      case Left(err) =>
        throw new RuntimeException(s"module_decode failed: $err")
    // {{Module}}'s 4 internal slots (index.bs:429-432); BuiltinSets/
    // ImportedStringModule stay empty/unset since the demo module doesn't use
    // either feature.
    val moduleObject = st.heap.allocRecord(
      "ModuleObject",
      List(
        "Module" -> decodedModule,
        "Bytes" -> Wasm(demoWasmBytesAL),
        "BuiltinSets" -> st.heap.allocList(Nil),
        "ImportedStringModule" -> Null,
      ),
    )
    val locals: MMap[Local, Value] =
      MMap.from(func.params.map(_.lhs).zip(List(moduleObject, importObj)))
    st.context = Context(func, locals)
    st.callStack = Nil

    // `**this**` in a WebIDL constructor's steps (e.g. {{Instance}}'s) refers
    // to a platform object that WebIDL's own "internally create a new object
    // implementing the interface" algorithm allocates *before* the
    // constructor steps run (webidl/index.bs, "interface object" [[Construct]]
    // — not mechanized here, see personal/constructor.md) — so, like
    // moduleObject above, the harness fabricates a placeholder directly rather
    // than actually running that preamble. {{Instance}}'s only
    // interface-specific slot is [[Exports]] (js-api/index.bs's "initialize an
    // instance object" sets it); harmless to bind even for entry points that
    // don't reference `this` at all.
    val thisObject = st.heap.allocRecord("Instance", List("Exports" -> Undef))
    st.globals += Global("this") -> thisObject

    val sep = "─" * 64
    println(s"invoke: ${config.entry}($moduleObject, $importObj)")
    println(sep)
    try
      EsInterpreter(st, wasmHost = Some(host))
      println(sep)
      st.globals.getOrElse(GLOBAL_RESULT, Undef)
    catch
      case e: (ESMetaError | NotImplementedError) =>
        println(sep)
        println(s"[${e.getClass.getSimpleName}] ${e.getMessage}")
        printCallStack(st)
        Undef
      case e: scala.MatchError =>
        println(sep)
        println(s"[MatchError — unhandled IR node] ${e.getMessage}")
        printCallStack(st)
        Undef
    finally connection.close()

  /** Prints the call stack (outermost first) plus the innermost frame's own
    * cursor — the exact node execution stopped at, not just which function.
    * Only tells the whole story for a failure in the *outermost* running
    * context, though: a reentrant call via `Interpreter.invokeCallable` (e.g.
    * servicing a Wasm `host_func_invoke`) restores the outer `st.context`
    * before its own exception propagates here, so a failure *inside* one of
    * those instead surfaces via that method's own stderr log, not this one.
    */
  private def printCallStack(st: State): Unit =
    val stack =
      (st.context.name :: st.callStack.map(_.context.name)).reverse
    println("Call stack (outermost first):")
    stack.zipWithIndex.foreach((f, i) => println(s"  ${"  " * i}$f"))
    println(s"Cursor: ${st.context.cursor}")

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "entry",
      StrOption((c, s) => c.entry = s),
      "the WJI function to invoke (default: instantiate_object).",
    ),
  )
  case class Config(
    var entry: String = "instantiate_object",
  )
}
