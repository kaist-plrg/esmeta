package esmeta.wji

import esmeta.es.builtin.AGENT_RECORD
import esmeta.state.*
import esmeta.wji.bridge.SpecTecWasmHost
import esmeta.wji.bridge.process.SpecTecProcess
import esmeta.wji.bridge.rpc.JsonRpcConnection

/** WJI runtime initialization, shared by every phase that runs compiled WJI IR
  * against a live SpecTec process (`wji-eval`, `wji-interp`).
  *
  * Builds the SpecTec process/connection/[[SpecTecWasmHost]] and seeds the
  * agent's associated store: "Each agent has an associated store. When a new
  * agent is created, its associated store is set to the result of
  * store_init()." (js-api/index.bs:334) — plain prose, not a `<div algorithm>`,
  * so it is never mechanized as a step. Once seeded here, spec references to it
  * (`the surrounding agent's associated store`, etc.) resolve to this heap slot
  * via ordinary field access — see Compiler's `SpecTerm("surrounding agent")`
  * case, which maps `the surrounding agent` to the same
  * [[esmeta.es.builtin.AGENT_RECORD]] global this writes into.
  */
object Initialize:

  /** starts a live SpecTec process, seeds `st`'s AGENT_RECORD's "associated
    * store" field in place, and returns the host + connection for the
    * interpreter run (the caller owns the connection and must close it).
    */
  def apply(st: State): (SpecTecWasmHost, JsonRpcConnection) =
    val process = SpecTecProcess.start()
    val connection = JsonRpcConnection.stdio(process)
    val host = SpecTecWasmHost(connection)

    host.call("store_init", Nil) match
      case Right(store) =>
        st.heap.update(
          NamedAddr(AGENT_RECORD),
          Str("associated store"),
          Wasm(store),
        )
      case Left(err) =>
        connection.close()
        throw new RuntimeException(s"store_init failed: $err")

    st.heap.update(
      NamedAddr(AGENT_RECORD),
      Str("Exported Function cache"),
      st.heap.allocMap(Nil),
    )

    st.heap.update(
      NamedAddr(AGENT_RECORD),
      Str("Memory object cache"),
      st.heap.allocMap(Nil),
    )

    st.heap.update(
      NamedAddr(AGENT_RECORD),
      Str("Table object cache"),
      st.heap.allocMap(Nil),
    )

    st.heap.update(
      NamedAddr(AGENT_RECORD),
      Str("Exported GC object cache"),
      st.heap.allocMap(Nil),
    )

    st.heap.update(
      NamedAddr(AGENT_RECORD),
      Str("Global object cache"),
      st.heap.allocMap(Nil),
    )

    st.heap.update(
      NamedAddr(AGENT_RECORD),
      Str("Tag object cache"),
      st.heap.allocMap(Nil),
    )

    st.heap.update(
      NamedAddr(AGENT_RECORD),
      Str("Exception object cache"),
      st.heap.allocMap(Nil),
    )

    st.heap.update(
      NamedAddr(AGENT_RECORD),
      Str("host value cache"),
      st.heap.allocMap(Nil),
    )

    (host, connection)
