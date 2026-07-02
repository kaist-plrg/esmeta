package esmeta.wji.bridge

import io.circe.Json
import io.circe.syntax.*
import java.util.concurrent.atomic.AtomicLong
import esmeta.wji.bridge.host.{HostFunction, WasmError, WasmHost}
import esmeta.wji.bridge.rpc.{Response, RpcError}
import esmeta.state.ALValue
import esmeta.state.util.ALValueJson.given
import esmeta.wji.bridge.rpc.JsonRpcConnection

/** [[WasmHost]] implementation backed by the SpecTec Wasm interpreter,
  * communicating over JSON-RPC via [[JsonRpcConnection]].
  *
  * Most methods are thin transport wrappers: encode the arguments as
  * [[ALValue]]/[[Json]] `params`, call `connection.request("<embedding function
  * name>", params)`, and decode the `result`/`error` into the `WasmHost` return
  * types. SpecTec owns validation (unknown function, arg type/arity
  * mismatches); this class only maps SpecTec-side errors to [[WasmError]].
  *
  * [[funcAlloc]] is special: it additionally registers `hostFunc` in
  * [[hostFunctions]] under a fresh id, encodes that id for SpecTec (e.g.
  * `ALValue.CaseV("HOSTFUNC", List(ALValue.TextV(id)))`), and includes it in
  * the `func_alloc` request params. The constructor registers a
  * `"host_func_invoke"` handler via `connection.onRequest` that looks up
  * [[hostFunctions]] by id, runs the [[HostFunction]], and encodes its result
  * back to SpecTec — this is how reentrant Wasm -> wjmeta calls are served.
  */
final class SpecTecWasmHost(connection: JsonRpcConnection) extends WasmHost:

  /** registry of [[HostFunction]]s passed to [[funcAlloc]], keyed by the id
    * sent to SpecTec as part of the `hostfunc` argument. Populated by
    * [[funcAlloc]], consulted by the `host_func_invoke` handler registered
    * below.
    */
  private val hostFunctions =
    scala.collection.concurrent.TrieMap.empty[String, HostFunction]

  /** Counter for the fresh ids under which [[funcAlloc]] registers
    * [[HostFunction]]s in [[hostFunctions]].
    */
  private val nextHostFuncId = new AtomicLong(0)

  // Serve reentrant `host_func_invoke` calls initiated by SpecTec: look up the
  // registered HostFunction by id, run it, and encode its result back.
  connection.onRequest {
    case ("host_func_invoke", params) =>
      val decoded =
        for
          id <- params.hcursor.get[String]("id")
          args <- params.hcursor.get[List[ALValue]]("args")
        yield (id, args)
      decoded match
        case Left(e) =>
          Left(
            RpcError(
              -32602,
              s"invalid host_func_invoke params: ${e.getMessage}",
              None,
            ),
          )
        case Right((id, args)) =>
          hostFunctions.get(id) match
            case None =>
              Left(RpcError(-32602, s"unknown host function id: $id", None))
            case Some(hostFunc) =>
              hostFunc(args) match
                case Right(results) =>
                  val out: ALValue = ALValue.ListV(results)
                  Right(out.asJson)
                case Left(WasmError.Trap(value)) =>
                  Left(
                    RpcError(-32000, "host function trap", Some(value.asJson)),
                  )
                case Left(WasmError.ProtocolError(msg)) =>
                  Left(RpcError(-32603, msg, None))
    case (other, _) =>
      Left(RpcError(-32601, s"unknown reverse call: $other", None))
  }

  // -- Transport helpers ------------------------------------------------------

  /** Send `method`/`params` to SpecTec and decode the single-[[ALValue]]
    * `result`, mapping transport failures / SpecTec errors to [[WasmError]]. A
    * SpecTec `error` whose `data` is an AL value becomes [[WasmError.Trap]].
    */
  private def call(method: String, params: Json): Either[WasmError, ALValue] =
    connection.request(method, params) match
      case Left(transportErr) => Left(WasmError.ProtocolError(transportErr))
      case Right(Response(_, Some(result), _, _)) =>
        result
          .as[ALValue]
          .left
          .map(e => WasmError.ProtocolError(s"bad result: $e"))
      case Right(Response(_, _, Some(err), _)) =>
        err.data.flatMap(_.as[ALValue].toOption) match
          case Some(al) => Left(WasmError.Trap(al))
          case None     => Left(WasmError.ProtocolError(err.message))
      case Right(_) =>
        Left(WasmError.ProtocolError("response had neither result nor error"))

  // -- Store --------------------------------------------------------------

  def storeInit(): Either[WasmError, ALValue] =
    call("store_init", Json.obj())

  // -- Modules --------------------------------------------------------------

  def moduleDecode(bytes: ALValue): Either[WasmError, ALValue] =
    call("module_decode", Json.obj("bytes" -> bytes.asJson))

  def moduleValidate(module: ALValue): Either[WasmError, ALValue] = ???

  def moduleInstantiate(
    store: ALValue,
    module: ALValue,
    externVals: List[ALValue],
  ): Either[WasmError, ALValue] = ???

  def moduleImports(module: ALValue): Either[WasmError, ALValue] = ???

  def moduleExports(module: ALValue): Either[WasmError, ALValue] = ???

  // -- Module instances -------------------------------------------------------

  def instanceExport(
    moduleInst: ALValue,
    name: ALValue,
  ): Either[WasmError, ALValue] = ???

  // -- Functions --------------------------------------------------------------

  /** Registers `hostFunc` in [[hostFunctions]] under a fresh id, then calls
    * `connection.request("func_alloc", ...)` with `hostfunc` encoded as
    * `CaseV("HOSTFUNC", List(TextV(id)))` so that SpecTec can later call it
    * back via `host_func_invoke`.
    */
  def funcAlloc(
    store: ALValue,
    defType: ALValue,
    hostFunc: HostFunction,
  ): Either[WasmError, ALValue] =
    val id = nextHostFuncId.getAndIncrement().toString
    hostFunctions(id) = hostFunc
    val hostfunc: ALValue = ALValue.CaseV("HOSTFUNC", List(ALValue.TextV(id)))
    call(
      "func_alloc",
      Json.obj(
        "store" -> store.asJson,
        "deftype" -> defType.asJson,
        "hostfunc" -> hostfunc.asJson,
      ),
    )

  def funcType(store: ALValue, funcAddr: ALValue): Either[WasmError, ALValue] =
    ???

  def funcInvoke(
    store: ALValue,
    funcAddr: ALValue,
    args: List[ALValue],
  ): Either[WasmError, ALValue] =
    val argList: ALValue = ALValue.ListV(args)
    call(
      "func_invoke",
      Json.obj(
        "store" -> store.asJson,
        "funcaddr" -> funcAddr.asJson,
        "args" -> argList.asJson,
      ),
    )

  // -- Tables -----------------------------------------------------------------

  def tableAlloc(
    store: ALValue,
    tableType: ALValue,
    ref: ALValue,
  ): Either[WasmError, ALValue] = ???

  def tableType(
    store: ALValue,
    tableAddr: ALValue,
  ): Either[WasmError, ALValue] = ???

  def tableRead(
    store: ALValue,
    tableAddr: ALValue,
    i: ALValue,
  ): Either[WasmError, ALValue] = ???

  def tableWrite(
    store: ALValue,
    tableAddr: ALValue,
    i: ALValue,
    ref: ALValue,
  ): Either[WasmError, ALValue] = ???

  def tableSize(
    store: ALValue,
    tableAddr: ALValue,
  ): Either[WasmError, ALValue] = ???

  def tableGrow(
    store: ALValue,
    tableAddr: ALValue,
    n: ALValue,
    ref: ALValue,
  ): Either[WasmError, ALValue] = ???

  // -- Memories -----------------------------------------------------------------

  def memAlloc(store: ALValue, memType: ALValue): Either[WasmError, ALValue] =
    ???

  def memType(store: ALValue, memAddr: ALValue): Either[WasmError, ALValue] =
    ???

  def memSize(store: ALValue, memAddr: ALValue): Either[WasmError, ALValue] =
    ???

  def memGrow(
    store: ALValue,
    memAddr: ALValue,
    n: ALValue,
  ): Either[WasmError, ALValue] = ???

  // -- Tags -----------------------------------------------------------------

  def tagAlloc(store: ALValue, tagType: ALValue): Either[WasmError, ALValue] =
    ???

  def tagType(store: ALValue, tagAddr: ALValue): Either[WasmError, ALValue] =
    ???

  // -- Exceptions -----------------------------------------------------------------

  def exnAlloc(
    store: ALValue,
    tagAddr: ALValue,
    vals: List[ALValue],
  ): Either[WasmError, ALValue] = ???

  def exnTag(store: ALValue, exnAddr: ALValue): Either[WasmError, ALValue] = ???

  def exnRead(store: ALValue, exnAddr: ALValue): Either[WasmError, ALValue] =
    ???

  // -- Globals -----------------------------------------------------------------

  def globalAlloc(
    store: ALValue,
    globalType: ALValue,
    value: ALValue,
  ): Either[WasmError, ALValue] = ???

  def globalType(
    store: ALValue,
    globalAddr: ALValue,
  ): Either[WasmError, ALValue] = ???

  def globalRead(
    store: ALValue,
    globalAddr: ALValue,
  ): Either[WasmError, ALValue] = ???

  def globalWrite(
    store: ALValue,
    globalAddr: ALValue,
    value: ALValue,
  ): Either[WasmError, ALValue] = ???

  // -- Values -----------------------------------------------------------------

  def refType(store: ALValue, ref: ALValue): Either[WasmError, ALValue] = ???

  def valDefault(valType: ALValue): Either[WasmError, ALValue] = ???

  // -- Matching -----------------------------------------------------------------

  def matchValType(
    valType1: ALValue,
    valType2: ALValue,
  ): Either[WasmError, ALValue] = ???

  def matchExternType(
    externType1: ALValue,
    externType2: ALValue,
  ): Either[WasmError, ALValue] = ???
