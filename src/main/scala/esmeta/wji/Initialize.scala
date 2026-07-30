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

  /** Spawns a fresh SpecTec `--server` process and wraps it in a
    * [[JsonRpcConnection]] — the expensive part of the one-arg [[apply]]
    * (process spawn + `.spectec` spec parse, ~10s measured), isolated so a
    * caller that runs many `State`s against the same spec (see
    * `esmeta.wji.WjiTest`/`EvalSpec`) can pay this cost once per test-suite run
    * instead of once per test case. The caller owns the returned connection and
    * must [[JsonRpcConnection.close]] it exactly once when done; repeated
    * per-test use on one connection is safe because the two-arg [[apply]] below
    * fully re-seeds everything test-scoped (`store_init`, the caches) on every
    * call.
    */
  def startProcess(): JsonRpcConnection =
    JsonRpcConnection.stdio(SpecTecProcess.start())

  /** Builds a fresh [[SpecTecWasmHost]] over an *already-running* `connection`
    * and seeds `st`'s AGENT_RECORD's "associated store" field in place — the
    * cheap, genuinely per-`State` part of initialization (a `store_init` RPC
    * round trip + empty-map cache seeding, ~1-2s), split out from
    * [[startProcess]] so it can be repeated per test case against one shared
    * process/connection without re-paying the process-spawn cost each time.
    * Safe to call repeatedly on the same `connection` across independent
    * `State`s *in sequence* (never concurrently — a [[JsonRpcConnection]] only
    * ever has one request in flight): [[SpecTecWasmHost]]'s constructor
    * overwrites the connection's `host_func_invoke` handler, so each call gets
    * its own isolated `hostFunctions` registry, and `store_init` fully replaces
    * the prior SpecTec-side store rather than reusing state from a previous
    * call. Does *not* close `connection` on failure — unlike the one-arg
    * [[apply]], this overload doesn't own the connection's lifecycle.
    */
  def apply(st: State, connection: JsonRpcConnection): SpecTecWasmHost =
    val host = SpecTecWasmHost(connection)

    host.call("store_init", Nil) match
      case Right(store) =>
        st.heap.update(
          NamedAddr(AGENT_RECORD),
          Str("associated store"),
          Wasm(store),
        )
      case Left(err) =>
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

    host

  /** starts a live SpecTec process, seeds `st`'s AGENT_RECORD's "associated
    * store" field in place, and returns the host + connection for the
    * interpreter run (the caller owns the connection and must close it).
    *
    * For callers that only ever run ONE `State` per JVM run (`wji-eval`,
    * `wji-interp` — see `esmeta.phase.WjiEval`/`WjiInterp`), one process per
    * run is already optimal, so this just composes [[startProcess]] and the
    * two-arg [[apply]] rather than something reused across calls — see
    * `esmeta.wji.WjiTest`/`EvalSpec` for the multi-`State`-per-process case
    * those were split out to support.
    */
  def apply(st: State): (SpecTecWasmHost, JsonRpcConnection) =
    val connection = startProcess()
    try (apply(st, connection), connection)
    catch
      case e: Throwable =>
        connection.close()
        throw e
