package esmeta.phase

import esmeta.*
import esmeta.util.*
import esmeta.cfg.CFG
import esmeta.cfgBuilder.CFGBuilder
import esmeta.error.ESMetaError
import esmeta.es.Initialize
import esmeta.es.builtin.{EXECUTION_STACK, realmAddr}
import esmeta.interpreter.{Interpreter => EsInterpreter}
import esmeta.ir.{Local, Program}
import esmeta.state.*
import esmeta.wji.bridge.SpecTecWasmHost
import esmeta.wji.bridge.process.SpecTecProcess
import esmeta.wji.bridge.rpc.JsonRpcConnection
import esmeta.wji.compiler.Compiler
import esmeta.wji.compiler.desugar.Desugar
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
  * So this phase runs a trivial *empty* script to completion first — which
  * pushes a context, wires up the realm/intrinsics, then pops the context again
  * — and then, reusing that same (heap-populated) [[State]], manually re-pushes
  * a fresh execution context referencing the now-fully-wired realm before
  * jumping directly into the target WJI function. This is a stand-in for "a
  * minimal JS driver called `entry`"; it does not run any actual JS source.
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
    val wjiProgram = Compiler.compile(Desugar.run(algorithms))
    val merged =
      Program(cfg.program.funcs ++ wjiProgram.funcs, cfg.program.spec)
    val mergedCfg = CFGBuilder(merged)
    given CFG = mergedCfg

    // run an empty script to completion so the real ECMA-262 bootstrap wires
    // up the realm/intrinsics; the execution context it pushed is popped
    // again by the time this returns, but the Realm Record it populated
    // lives on in the heap.
    val st = EsInterpreter(Initialize(mergedCfg).from(""))

    // re-push a fresh execution context referencing that now-populated realm
    // so `the current Realm Record` (and friends) resolve for the call below.
    val execContext = st.heap.allocRecord(
      "ExecutionContext",
      List("Realm" -> realmAddr, "Function" -> Null, "ScriptOrModule" -> Null),
    )
    st.heap.push(NamedAddr(EXECUTION_STACK), execContext, true)

    val func = mergedCfg.getFunc(config.entry)
    // a placeholder decoded `module` (Wasm Core Spec 1.4-syntax.modules):
    // a record so field reads like `|module|.[=imports=]` can be projected
    // directly (see State.apply's Wasm(StrV(...)) case) without a round trip
    // to SpecTec.
    val placeholderModule = Wasm(
      ALValue.StrV(
        List(
          "types" -> ALValue.ListV(Nil),
          "imports" -> ALValue.ListV(Nil),
          "tags" -> ALValue.ListV(Nil),
          "globals" -> ALValue.ListV(Nil),
          "mems" -> ALValue.ListV(Nil),
          "tables" -> ALValue.ListV(Nil),
          "funcs" -> ALValue.ListV(Nil),
          "datas" -> ALValue.ListV(Nil),
          "elems" -> ALValue.ListV(Nil),
          "start" -> ALValue.OptV(None),
          "exports" -> ALValue.ListV(Nil),
        ),
      ),
    )
    // {{Module}}'s 4 internal slots (index.bs:429-432); filled with
    // placeholder values just to see how far execution gets past them.
    val moduleObject = st.heap.allocRecord(
      "ModuleObject",
      List(
        "Module" -> placeholderModule,
        "Bytes" -> Wasm(ALValue.ListV(Nil)),
        "BuiltinSets" -> st.heap.allocList(Nil),
        "ImportedStringModule" -> Undef,
      ),
    )
    val importObject = st.heap.allocRecord("ImportObject")
    val locals: MMap[Local, Value] =
      MMap.from(func.params.map(_.lhs).zip(List(moduleObject, importObject)))
    st.context = Context(func, locals)
    st.callStack = Nil

    val process = SpecTecProcess.start()
    val connection = JsonRpcConnection.stdio(process)
    val host = SpecTecWasmHost(connection)

    val sep = "─" * 64
    println(s"invoke: ${config.entry}($moduleObject, $importObject)")
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

  private def printCallStack(st: State): Unit =
    val stack =
      (st.context.name :: st.callStack.map(_.context.name)).reverse
    println("Call stack (outermost first):")
    stack.zipWithIndex.foreach((f, i) => println(s"  ${"  " * i}$f"))

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
