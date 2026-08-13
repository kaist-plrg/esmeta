package esmeta.wji

import esmeta.cfg.CFG
import esmeta.es.builtin.{AGENT_RECORD, HOST_DEFINED}
import esmeta.ir.Global
import esmeta.state.*
import esmeta.wji.bridge.SpecTecWasmHost
import esmeta.wji.bridge.process.SpecTecProcess
import esmeta.wji.bridge.rpc.JsonRpcConnection
import esmeta.wji.extractor.Extractor
import esmeta.wji.spec.Spec
import esmeta.wji.lang.{Definition, DefinitionKind}
import esmeta.wji.lang.{Operation => WjiOperation}
import esmeta.wji.lang.{Attribute => WjiAttribute}
import esmeta.wji.lang.{Param => WjiParam}
import esmeta.wji.lang.ExtendedAttribute

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
  def apply(
    st: State,
    spec: Spec,
    connection: JsonRpcConnection,
  ): SpecTecWasmHost =
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

    st.heap.update(
      NamedAddr(AGENT_RECORD),
      Str("Function Import List"),
      st.heap.allocList(Nil),
    )

    seedHostDefined(st, spec)

    host

  /** Builds a runtime record for the extracted `WebAssembly` [[Definition]] and
    * writes it into `HOST_DEFINED.WebAssembly` — the namespace record
    * `create_a_namespace_object` (see `manuals/rule.json`'s "Create any
    * host-defined global object properties on _global_." patch) builds the real
    * `WebAssembly` global object from, instead of a hardcoded intrinsic.
    *
    * Each runtime record mirrors its `esmeta.wji.lang` counterpart's own field
    * shape 1:1 (`id`/`params`/`returnType`/`kind`/`extendedAttributes`, ...),
    * so a mechanized WebIDL algorithm reads exactly the data the real spec text
    * describes — no pre-filtered/pre-computed field (e.g. "the regular
    * operations") is baked in here; that's the mechanized algorithms' own job
    * once they read `kind`/`extendedAttributes` themselves. The one field with
    * no `esmeta.wji.lang` counterpart at all is `operation`'s `methodSteps`:
    * the actual compiled closure implementing that operation's algorithm body
    * (looked up by `id` in the already-merged `cfg.fnameMap`), since nothing
    * about the *syntactic* `Definition` extraction knows how its members ended
    * up compiled. Requires `st.cfg` to be the merged CFG (WJI funcs included) —
    * this must run after that merge, not before.
    */
  private def seedHostDefined(st: State, spec: Spec): Unit =
    given CFG = st.cfg

    def extAttrRecord(ea: ExtendedAttribute): Addr =
      st.allocRecord(
        "extendedAttribute",
        List(
          "id" -> Str(ea.id),
          "value" -> ea.value.fold[Value](Undef)(Str(_)),
        ),
      )

    def paramRecord(p: WjiParam): Addr =
      st.allocRecord(
        "param",
        List(
          "ty" -> Str(p.ty),
          "optional" -> Bool(p.optional),
          "default" -> Str(p.default),
          "extendedAttributes" ->
          st.allocList(p.extAttribute.map(extAttrRecord)),
        ),
      )

    def operationRecord(op: WjiOperation): Addr =
      st.allocRecord(
        "operation",
        List(
          "id" -> Str(op.id),
          "params" -> st.allocList(op.params.map(paramRecord)),
          "returnType" -> Str(op.ret),
          "kind" -> Enum(op.kind.toString),
          "extendedAttributes" -> st.allocList(op.extAttr.map(extAttrRecord)),
          "methodSteps" -> st.cfg.fnameMap
            .get(op.id)
            .fold[Value](Undef)(f => Clo(f, Map())),
        ),
      )

    def attributeRecord(attr: WjiAttribute): Addr =
      st.allocRecord(
        "attribute",
        List(
          "id" -> Str(attr.id),
          "ty" -> Str(attr.ty),
          "readonly" -> Bool(attr.readonly),
          "kind" -> Enum(attr.kind.toString),
          "extendedAttributes" ->
          st.allocList(attr.extAttr.map(extAttrRecord)),
        ),
      )

    def definitionRecord(d: Definition): Addr =
      val members = d.members.map {
        case op: WjiOperation   => operationRecord(op)
        case attr: WjiAttribute => attributeRecord(attr)
        case _                  => ???
      }
      st.allocRecord(
        d.kind.toString,
        List(
          "id" -> Str(d.name),
          "members" -> st.allocList(members),
          "kind" -> Enum(d.kind.toString),
          "extendedAttributes" -> st.allocList(d.extAttr.map(extAttrRecord)),
        ),
      )

    spec.definitions.foreach { definition =>
      st.heap.update(
        NamedAddr(HOST_DEFINED),
        Str(definition.name),
        definitionRecord(definition),
      )
    }

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
  def apply(st: State, spec: Spec): (SpecTecWasmHost, JsonRpcConnection) =
    val connection = startProcess()
    try (apply(st, spec, connection), connection)
    catch
      case e: Throwable =>
        connection.close()
        throw e
