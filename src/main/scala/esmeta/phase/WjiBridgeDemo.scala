package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.cfgBuilder.CFGBuilder
import esmeta.interpreter.{Interpreter => EsInterpreter}
import esmeta.ir.*
import esmeta.state.*
import esmeta.util.*
import esmeta.wji.bridge.SpecTecWasmHost
import esmeta.wji.bridge.process.SpecTecProcess
import esmeta.wji.bridge.rpc.JsonRpcConnection
import esmeta.state.ALNum
import esmeta.state.ALValue
import scala.collection.mutable.{Map => MMap}

/** `wji-bridge-demo` phase
  *
  * Wires the unified `esmeta.interpreter.Interpreter` together with the SpecTec
  * WasmHost (over JSON-RPC) to run a hand-built `esmeta.ir` program (replaces
  * the old `wjmeta-app` `Main`, and — now that WJI and ES share one
  * IR/interpreter — the separate WJI-only interpreter this demo used before).
  * The program exercises three levels of crossing the JSON-RPC boundary on one
  * channel:
  *
  *   1. wji -> SpecTec : invoke a Wasm function (func_invoke) 2. SpecTec -> wji
  *      : that function calls a host function (hostFn) 3. wji -> SpecTec : the
  *      host function itself calls an embedding function (store_init)
  *      *mid-callback*, while SpecTec is still blocked awaiting hostFn's
  *      result.
  *
  * `hostFn`/`runHost` are merged into the same CFG as the real ES spec
  * functions (so `hostFn`'s `clo<"ToBoolean">` call below resolves against a
  * real ES abstract operation, exactly like any other ES-to-ES call).
  *
  * Requires a live SpecTec process on `PATH` (see [[SpecTecProcess]]).
  */
case object WjiBridgeDemo extends Phase[CFG, Unit] {
  val name = "wji-bridge-demo"
  val help = "runs the unified interpreter against a live SpecTec WasmHost."

  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit =
    val process = SpecTecProcess.start()
    val connection = JsonRpcConnection.stdio(process)
    val host = SpecTecWasmHost(connection)

    // Exercise module_decode + module_imports end-to-end before the rest of
    // the demo: decode a minimal module (one type, one func import of
    // "console"."log") and confirm the import comes back out.
    import ALValue.*
    val moduleBytes = List(
      0, 97, 115, 109, 1, 0, 0, 0, // \0asm, version 1
      1, 5, 1, 96, 1, 127, 0, // type section: type 0 = (func (param i32))
      2, 15, 1, 7, 99, 111, 110, 115, 111, 108, 101, 3, 108, 111, 103, 0, 0,
      // import section: "console" "log" func (type 0)
    )
    val bytesVal: ALValue =
      ListV(moduleBytes.map(n => NumV(ALNum.Nat(n))))
    (for
      module <- host.call("module_decode", List(bytesVal))
      imports <- host.call("module_imports", List(module))
    yield imports) match
      case Right(imports) => println(s"module_imports -> $imports")
      case Left(err) =>
        throw new RuntimeException(s"module_imports demo failed: $err")

    //   global %Store = { let s = store_init(); return s }
    //   global %Faddr = { return -1 }
    //   global %Count = { return 0 }
    //
    //   function hostFn(arg):
    //     if %Count < 10 then
    //       let %Count = %Count + 1
    //       print %Count
    //       let inv = func_invoke(%Store, %Faddr, [arg])
    //    return [arg]
    //
    //   function runHost(deftype, arg):
    //     let alloc  = func_alloc(%Store, deftype, hostFn)
    //     let store = alloc.0
    //     let faddr = alloc.1
    //     %Store := store
    //     %Faddr := faddr
    //     let inv = func_invoke(%Store, faddr, [arg])
    //     let store = inv.0
    //     let result = inv.1
    //     %Store := store
    //     print result
    //     return result
    val hostFn = Func(
      main = false,
      kind = FuncKind.AbsOp,
      name = "hostFn",
      params = List(Param(Name("arg"))),
      retTy = UnknownType,
      body = ISeq(
        List(
          IIf(
            EBinary(BOp.Lt, ERef(Global("Count")), EMath(10)),
            ISeq(
              List(
                IAssign(
                  Global("Count"),
                  EBinary(BOp.Add, ERef(Global("Count")), EMath(1)),
                ),
                IPrint(ERef(Global("Count"))),
                ICallEmbed(
                  Name("inv"),
                  "func_invoke",
                  List(
                    ERef(Global("Store")),
                    ERef(Global("Faddr")),
                    EList(List(ERef(Name("arg")))),
                  ),
                ),
              ),
            ),
            ISeq(
              List(
                ICall(
                  Name("b"),
                  EClo("ToBoolean", Nil),
                  List(ERef(Global("Count"))),
                ),
                IPrint(ERef(Name("b"))),
              ),
            ),
          ),
          IReturn(EList(List(ERef(Name("arg"))))),
        ),
      ),
    )
    val runHost = Func(
      main = false,
      kind = FuncKind.AbsOp,
      name = "runHost",
      params = List(Param(Name("deftype")), Param(Name("arg"))),
      retTy = UnknownType,
      body = ISeq(
        List(
          ICallEmbed(
            Name("alloc"),
            "func_alloc",
            List(
              ERef(Global("Store")),
              ERef(Name("deftype")),
              EClo("hostFn", Nil),
            ),
          ),
          ILet(Name("store"), EProj(ERef(Name("alloc")), 0)),
          ILet(Name("faddr"), EProj(ERef(Name("alloc")), 1)),
          IAssign(Global("Store"), ERef(Name("store"))),
          IAssign(Global("Faddr"), ERef(Name("faddr"))),
          ICallEmbed(
            Name("inv"),
            "func_invoke",
            List(
              ERef(Global("Store")),
              ERef(Name("faddr")),
              EList(List(ERef(Name("arg")))),
            ),
          ),
          ILet(Name("store"), EProj(ERef(Name("inv")), 0)),
          ILet(Name("result"), EProj(ERef(Name("inv")), 1)),
          IAssign(Global("Store"), ERef(Name("store"))),
          IPrint(ERef(Name("result"))),
          IReturn(ERef(Name("result"))),
        ),
      ),
    )
    // merge into the real ES CFG's functions so `clo<"ToBoolean">` resolves
    val merged =
      Program(cfg.program.funcs ++ List(hostFn, runHost), cfg.program.spec)
    val bridgeCfg = CFGBuilder(merged)

    // Host function type `[i32] -> [i32]` as an AL deftype, mirroring host.ml's
    // `create_funcinst` dtype (Wasm 3.0 representation):
    //   _DEF(REC[ SUB(?FINAL, [], (I32* -> I32*)) ], 0)
    val i32 = CaseV("I32", Nil)
    val ftype = CaseV("->", List(ListV(List(i32)), ListV(List(i32))))
    val sub =
      CaseV("SUB", List(OptV(Some(CaseV("FINAL", Nil))), ListV(Nil), ftype))
    val deftype = CaseV(
      "_DEF",
      List(CaseV("REC", List(ListV(List(sub)))), NumV(ALNum.Nat(0))),
    )

    // Invocation argument: the Wasm value `(i32.const 42)`.
    val arg = CaseV("CONST", List(i32, NumV(ALNum.Nat(42))))

    // module-level globals: %Store is seeded once by store_init() before any
    // function runs, then threaded through the global instead of through
    // locals; there's no `esmeta.ir` equivalent of WJI's old `GlobalDecl`
    // initializer instructions, so it's just computed directly here.
    val initStore = host.call("store_init", Nil) match
      case Right(av) => Wasm(av)
      case Left(err) =>
        throw new RuntimeException(s"store_init failed: $err")
    val globals: MMap[Global, Value] = MMap(
      Global("Store") -> initStore,
      Global("Faddr") -> Math(-1),
      Global("Count") -> Math(0),
    )

    val func = bridgeCfg.getFunc("runHost")
    val locals: MMap[Local, Value] =
      MMap(Name("deftype") -> Wasm(deftype), Name("arg") -> Wasm(arg))
    val st =
      State(bridgeCfg, Context(func, locals), globals = globals, heap = Heap())
    EsInterpreter(st, wasmHost = Some(host))
    val result = st.globals.getOrElse(GLOBAL_RESULT, Undef)
    println(s"runHost -> $result")

    connection.close()

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = Nil
  case class Config()
}
