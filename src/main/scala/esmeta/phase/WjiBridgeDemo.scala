package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.util.*
import esmeta.wji.bridge.SpecTecWasmHost
import esmeta.wji.bridge.process.SpecTecProcess
import esmeta.wji.bridge.rpc.JsonRpcConnection
import esmeta.wji.state.{ALNum, ALValue, WjValue}
import esmeta.wji.ir.*
import esmeta.wji.interpreter.Interpreter

/** `wji-bridge-demo` phase
  *
  * Wires the WJI interpreter together with the SpecTec WasmHost (over JSON-RPC)
  * to run a hand-built IR program (replaces the old `wjmeta-app` `Main`). The
  * program exercises three levels of crossing the JSON-RPC boundary on one
  * channel:
  *
  *   1. wji -> SpecTec : invoke a Wasm function (func_invoke) 2. SpecTec -> wji
  *      : that function calls a host function (hostFn) 3. wji -> SpecTec : the
  *      host function itself calls an embedding function (store_init)
  *      *mid-callback*, while SpecTec is still blocked awaiting hostFn's
  *      result.
  *
  * Requires a live SpecTec process on `PATH` (see [[SpecTecProcess]]).
  */
case object WjiBridgeDemo extends Phase[CFG, Unit] {
  val name = "wji-bridge-demo"
  val help = "runs the WJI interpreter against a live SpecTec WasmHost."

  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit =
    val process = SpecTecProcess.start()
    val connection = JsonRpcConnection.stdio(process)
    val host = SpecTecWasmHost(connection)

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
      name = "hostFn",
      params = List(Param(Name("arg"))),
      body = ISeq(
        List(
          IIf(
            EBinary(BOp.Lt, ERef(Global("Count")), ENum(10)),
            ISeq(
              List(
                IAssign(
                  Global("Count"),
                  EBinary(BOp.Add, ERef(Global("Count")), ENum(1)),
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
            ISeq(List(
              ICall(Name("b"), EClo("ToBoolean"), List(ERef(Global("Count")))),
              IPrint(ERef(Name("b"))),
            )),
          ),
          IReturn(EList(List(ERef(Name("arg"))))),
        ),
      ),
    )
    // Module-level Wasm store, seeded once by store_init() before any function
    // runs, then threaded through the %Store global instead of through locals.
    val storeGlobal = GlobalDecl(
      "Store",
      ISeq(
        List(
          ICallEmbed(Name("s"), "store_init", Nil),
          IReturn(ERef(Name("s"))),
        ),
      ),
    )
    val addrGlobal = GlobalDecl(
      "Faddr",
      IReturn(ENum(-1)),
    )
    val countGlobal = GlobalDecl(
      "Count",
      IReturn(ENum(0)),
    )
    val runHost = Func(
      name = "runHost",
      params = List(Param(Name("deftype")), Param(Name("arg"))),
      body = ISeq(
        List(
          ICallEmbed(
            Name("alloc"),
            "func_alloc",
            List(ERef(Global("Store")), ERef(Name("deftype")), EClo("hostFn")),
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
    val program =
      Program(List(hostFn, runHost), List(storeGlobal, countGlobal, addrGlobal))

    // Host function type `[i32] -> [i32]` as an AL deftype, mirroring host.ml's
    // `create_funcinst` dtype (Wasm 3.0 representation):
    //   _DEF(REC[ SUB(?FINAL, [], (I32* -> I32*)) ], 0)
    import ALValue.*
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

    val interpreter = Interpreter(program, host, Some(cfg))
    val result = interpreter.invoke(
      "runHost",
      List(WjValue.Wasm(deftype), WjValue.Wasm(arg)),
    )
    println(s"runHost -> $result")

    connection.close()

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = Nil
  case class Config()
}
