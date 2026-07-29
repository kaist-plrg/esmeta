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
  * [[call]] is a thin transport wrapper shared by every embedding function
  * except [[funcAlloc]]: zip `args` against `WasmHost.paramNames(name)` into a
  * named JSON object, `connection.request(name, params)`, and decode the
  * `result`/`error` into the `WasmHost` return types. SpecTec owns validation
  * (unknown function, arg type/arity mismatches); this class only maps
  * SpecTec-side errors to [[WasmError]].
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
          store <- params.hcursor.get[ALValue]("store")
          args <- params.hcursor.get[List[ALValue]]("args")
        yield (id, store, args)
      decoded match
        case Left(e) =>
          Left(
            RpcError(
              -32602,
              s"invalid host_func_invoke params: ${e.getMessage}",
              None,
            ),
          )
        case Right((id, store, args)) =>
          hostFunctions.get(id) match
            case None =>
              Left(RpcError(-32602, s"unknown host function id: $id", None))
            case Some(hostFunc) =>
              hostFunc(store, args) match
                case Right((newStore, results)) =>
                  val out: ALValue =
                    ALValue.TupV(List(newStore, ALValue.ListV(results)))
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
  private def request(
    method: String,
    params: Json,
  ): Either[WasmError, ALValue] =
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

  /** Generic dispatch for every embedding function except `func_alloc` (see
    * class doc / [[funcAlloc]]): zips `args` against
    * `WasmHost.paramNames(name)` to build the named JSON params, then sends
    * `name` itself as the JSON-RPC method (the two always match — a wire-level
    * mirror of the Wasm Embedding API's own function names).
    */
  def call(name: String, args: List[ALValue]): Either[WasmError, ALValue] =
    val paramNames = WasmHost.paramNames.getOrElse(
      name,
      throw new IllegalArgumentException(s"unknown embedding function: $name"),
    )
    val fields = paramNames.zip(args).map((n, a) => n -> a.asJson)
    request(name, Json.obj(fields*))

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
    request(
      "func_alloc",
      Json.obj(
        "store" -> store.asJson,
        "deftype" -> defType.asJson,
        "hostfunc" -> hostfunc.asJson,
      ),
    )
