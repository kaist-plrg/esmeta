package esmeta.wji.bridge.rpc

import io.circe.{Decoder, Encoder, Json}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

/** Generic JSON-RPC 2.0 message envelopes used on the bidirectional stdio
  * connection between wjmeta-bridge and the spectec-server process.
  * Method-specific `params`/`result` are kept as raw [[Json]] and
  * decoded/encoded as [[esmeta.wji.state.ALValue]]:
  *
  *   - wjmeta -> SpecTec: `method` is the name of a Wasm Embedding function
  *     (`module_decode`, `func_invoke`, ...), matching a method of
  *     `esmeta.wji.bridge.host.WasmHost` 1:1. `params`/`result` are the AL
  *     values of that function's arguments/results.
  *   - SpecTec -> wjmeta (reentrant calls): `method = "host_func_invoke"`,
  *     `params = {"id": <host function id>, "args": [ALValue, ...]}`, `result`
  *     is the AL value list returned by the
  *     `esmeta.wji.bridge.host.HostFunction` registered via `funcAlloc`.
  *
  * Either side may send a [[Request]] on the same connection; each side matches
  * [[Response]]s against the `id`s of requests *it* sent.
  */
final case class Request(
  jsonrpc: String,
  method: String,
  params: Json,
  id: Long,
)
object Request:
  given Decoder[Request] = deriveDecoder
  given Encoder[Request] = deriveEncoder
  def apply(method: String, params: Json, id: Long): Request =
    Request("2.0", method, params, id)

final case class RpcError(code: Int, message: String, data: Option[Json] = None)
object RpcError:
  given Decoder[RpcError] = deriveDecoder
  given Encoder[RpcError] = deriveEncoder

final case class Response(
  jsonrpc: String,
  result: Option[Json],
  error: Option[RpcError],
  id: Long,
)
object Response:
  given Decoder[Response] = deriveDecoder
  given Encoder[Response] = deriveEncoder
