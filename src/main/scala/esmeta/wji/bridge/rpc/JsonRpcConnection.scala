package esmeta.wji.bridge.rpc

import io.circe.{Json, parser}
import io.circe.syntax.*
import java.io.{BufferedReader, InputStreamReader, PrintStream}
import java.util.concurrent.atomic.AtomicLong
import esmeta.wji.bridge.rpc.{Request, Response, RpcError}

/** Bidirectional JSON-RPC 2.0 peer over a line-delimited JSON transport (stdio
  * to/from the spectec-server process).
  *
  * Both wjmeta-bridge and spectec-server can send [[Request]]s on this
  * connection:
  *   - wjmeta -> SpecTec: embedding function calls (`module_decode`,
  *     `func_invoke`, ...), sent via [[request]].
  *   - SpecTec -> wjmeta: reentrant `host_func_invoke` calls, delivered to the
  *     handler registered via [[onRequest]].
  *
  * Inbound lines are distinguished by shape: a value with a `method` field is a
  * request (from the peer); a value with `result`/`error` is a response to a
  * request *this side* previously sent. Each side tracks its own outgoing
  * request ids independently, so no coordination of id spaces is needed.
  */
trait JsonRpcConnection:
  /** Send a request to the peer and block for the matching [[Response]]. */
  def request(method: String, params: Json): Either[String, Response]

  /** Register the handler for requests initiated by the peer (e.g.
    * `host_func_invoke`). The handler receives the request's `method` and
    * `params` and returns either an [[RpcError]] or a `result` [[Json]].
    */
  def onRequest(handler: (String, Json) => Either[RpcError, Json]): Unit

  /** true once an inbound request's handling has thrown before a response line
    * could be written for it -- the peer is left blocked waiting for that
    * response forever, so the connection is unusable for anything afterward
    * (see [[StdioJsonRpcConnection.serve]]). A caller that reuses one
    * connection across many independent calls (e.g. `esmeta.wji.EvalSpec`)
    * should only pay to respawn the process when this is true: an exception
    * that never went through [[onRequest]]'s handler (e.g. a plain interpreter
    * error unrelated to any pending `host_func_invoke`) leaves the peer in no
    * different a state than a normal [[request]] return, so the connection is
    * still perfectly reusable.
    */
  def isPoisoned: Boolean

  def close(): Unit

/** [[JsonRpcConnection]] over a [[esmeta.wji.bridge.process.SpecTecProcess]]'s
  * stdio: one JSON value per line in each direction.
  *
  * There is no background reader thread. Since SpecTec only ever sends a
  * request (`host_func_invoke`) *while servicing a request wjmeta initiated*,
  * [[request]] itself drives the read loop: after sending, it reads lines and,
  * per line, either returns the matching [[Response]] or — if the line is an
  * inbound request — dispatches it to [[requestHandler]], writes the response,
  * and keeps reading. A handler may call [[request]] again; the nested call
  * runs its own pump on the stack, so host -> wasm -> host (and host ->
  * embedding) reentrancy works to arbitrary depth on the single channel. This
  * mirrors `spectec-server`'s `send_request` pump.
  */
private final class StdioJsonRpcConnection(
  process: esmeta.wji.bridge.process.SpecTecProcess,
) extends JsonRpcConnection:

  private val out = new PrintStream(process.stdin, true, "UTF-8")
  private val in = new BufferedReader(
    new InputStreamReader(process.stdout, "UTF-8"),
  )

  private val nextId = new AtomicLong(0)

  private var requestHandler: (String, Json) => Either[RpcError, Json] =
    (_, _) => Left(RpcError(-32601, "no handler registered", None))

  private var poisoned = false
  def isPoisoned: Boolean = poisoned

  /** Dispatch one inbound request to [[requestHandler]] and write its response.
    * If [[requestHandler]] throws, the peer never gets that response and is
    * left blocked forever -- mark [[poisoned]] before rethrowing so a caller
    * like `EvalSpec` knows this connection can no longer be reused, then
    * propagate the original exception unchanged.
    */
  private def serve(json: Json): Unit =
    val obj = json.hcursor
    val method = obj.get[String]("method").getOrElse("")
    val params = obj.get[Json]("params").getOrElse(Json.obj())
    val id = obj.get[Long]("id").getOrElse(0L)
    val response =
      try
        requestHandler(method, params) match
          case Right(result) => Response("2.0", Some(result), None, id)
          case Left(err)     => Response("2.0", None, Some(err), id)
      catch
        case e: Throwable =>
          poisoned = true
          throw e
    out.println(response.asJson.noSpaces)

  /** Read lines until the response to `id` arrives, serving any inbound
    * requests (e.g. a reentrant `host_func_invoke`) met in the meantime.
    */
  private def pump(id: Long): Either[String, Response] =
    var result: Option[Either[String, Response]] = None
    while result.isEmpty do
      val line = in.readLine()
      if line == null then result = Some(Left("connection closed"))
      else if line.nonEmpty then
        parser.parse(line) match
          case Left(parseErr) =>
            result = Some(Left(s"Parse error: ${parseErr.message}"))
          case Right(json) =>
            val obj = json.hcursor
            if obj.keys.exists(_.exists(_ == "method")) then
              serve(json) // inbound request -> handle it, then keep pumping
            else
              json.as[Response] match
                case Right(resp) if resp.id == id => result = Some(Right(resp))
                case Right(resp) =>
                  result = Some(
                    Left(s"unexpected response id ${resp.id} (awaiting $id)"),
                  )
                case Left(decodeErr) =>
                  result = Some(Left(s"malformed response: $decodeErr ($line)"))
    result.get

  def request(method: String, params: Json): Either[String, Response] =
    val id = nextId.incrementAndGet()
    out.println(Request(method, params, id).asJson.noSpaces)
    pump(id)

  def onRequest(handler: (String, Json) => Either[RpcError, Json]): Unit =
    requestHandler = handler

  def close(): Unit =
    process.close()

object JsonRpcConnection:
  def stdio(
    process: esmeta.wji.bridge.process.SpecTecProcess,
  ): JsonRpcConnection =
    new StdioJsonRpcConnection(process)
