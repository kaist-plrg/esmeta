package esmeta.interpreter

import esmeta.cfg.Call
import esmeta.error.WasmHostFailure
import esmeta.ir.GLOBAL_AGENT_RECORD
import esmeta.state.*
import esmeta.state.util.{fromALNum, toAL}

/** Syncs JS `ArrayBuffer` bytes with a wasm store's linear memory across the
  * ESMeta (JVM) / SpecTec (separate OCaml process) boundary. True aliasing —
  * what js-api's own "identified with" phrasing assumes — is architecturally
  * impossible here, since the two only ever talk over JSON-RPC; instead, the
  * whole byte content of every live memory is resynced at each of the two
  * points where JS and wasm execution can actually observe each other:
  * [[Interpreter.callEmbedding]]'s `func_invoke`/`module_instantiate` case (JS
  * calls into wasm) and [[Interpreter.toHostFunc]] (wasm reentrantly calls back
  * into JS). Both call sites now have an explicit `store` value in hand
  * (`create a host function`'s closure was `SpecPatch`-corrected to take/
  * return `state` explicitly, same as every other store-touching js-api
  * algorithm — see `docs/spec_inconsistencies.md` #7), so both go through the
  * same pure-local, RPC-free pair below: [[pushMemoriesIntoStore]]/
  * [[pullMemoriesFromStore]]. No separate `mem_read_bytes`/`mem_write_bytes`
  * RPC-based pair is needed any more.
  *
  * Takes the owning [[Interpreter]] rather than just its [[State]] because
  * [[applyPulledBytes]]'s growable-memory path needs a genuine reentrant call
  * (`invokeCallable`) to reuse "refresh the Memory buffer"'s own mechanized
  * branching — see [[invokeNamedWji]].
  */
private[interpreter] class WasmMemoryBridge(interp: Interpreter):
  private def st: State = interp.st

  /** the value currently bound to the global `AGENT_RECORD` variable — always
    * an [[Addr]] once `esmeta.wji.Initialize` has run, but read generically
    * (matches how every other reference to it goes through the `Global`
    * variable rather than hardcoding the `NamedAddr` string).
    */
  private def agentRecordAddr: Value = st(GLOBAL_AGENT_RECORD)

  /** every live `(memaddr, memoryObjAddr)` pair in the surrounding agent's
    * "Memory object cache" (index.bs:852) — every `Memory` JS object created so
    * far via `initialize_a_memory_object`.
    */
  private def memoryObjectCacheEntries(): Iterable[(Value, Value)] =
    val cacheAddr = st(agentRecordAddr, Str("Memory object cache")).asAddr
    st(cacheAddr) match
      case MapObj(map) => map
      case other =>
        throw WasmHostFailure(
          s"Memory object cache: expected a map, got $other",
        )

  /** `memoryObjAddr`'s `[[BufferObject]].[[ArrayBufferData]]` — the heap
    * [[Addr]] the actual byte-content `ListObj` lives at, or `None` if detached
    * (`[[ArrayBufferData]]` is `null`).
    */
  private def arrayBufferDataAddr(memoryObjAddr: Value): Option[Addr] =
    val bufferAddr = st(memoryObjAddr, Str("BufferObject")).asAddr
    st(bufferAddr, Str("ArrayBufferData")) match
      case addr: Addr => Some(addr)
      case _          => None

  /** invoke the already-mechanized WJI algorithm `fname` (its real compiled,
    * underscored `cfg.fnameMap` name, e.g. `"refresh_the_memory_buffer"`) as an
    * ordinary reentrant call — see [[Interpreter.invokeCallable]]. Used by
    * [[applyPulledBytes]] to reuse "refresh the Memory buffer"'s own
    * `IsFixedLengthArrayBuffer`/`DetachArrayBuffer` branching instead of
    * duplicating it here.
    */
  private def invokeNamedWji(
    fname: String,
    args: List[Value],
    call: Call,
  ): Value =
    interp.invokeCallable(Clo(st.cfg.getFunc(fname), Map()), args, call)

  private def memaddrIndex(memaddr: Value): Int = toAL(st, memaddr) match
    case ALValue.NumV(ALNum.Nat(n)) => n.toInt
    case other => throw WasmHostFailure(s"expected a nat memaddr, got $other")

  /** [[toAL]]'s inverse re-tagging for a single byte — plain [[toAL]] tags a
    * freshly-converted [[Math]] value `` `Int ``, but the mechanized Wasm
    * bytecode interpreter's byte-sequence ops (`inv_ibytes`, used by every
    * memory load instruction) only accept `` `Nat ``-tagged bytes and throw
    * `ArgMismatch` otherwise — confirmed empirically (`inv_ibytes: invalid
    * byte: +0`, `` `Int ``'s own string rendering of 0) when
    * [[pushMemoriesIntoStore]] first shipped bytes through plain [[toAL]].
    * Applied to every byte before it goes into the `MEMS[i].BYTES` patch below.
    */
  private def toALByte(v: Value): ALValue = toAL(st, v) match
    case ALValue.NumV(ALNum.Int(n)) => ALValue.NumV(ALNum.Nat(n))
    case av                         => av

  /** Replace `storeVal`'s `MEMS[i].BYTES` field, for every live memory `i` in
    * the "Memory object cache", with that memory's *current* JS-side
    * `ArrayBuffer` bytes — a pure local `ALValue`-tree edit, no RPC round trip.
    * This is the only way the JS→wasm push direction can actually work for
    * `func_invoke`/`module_instantiate`: their `store` argument is captured as
    * a local variable in the *caller* algorithm well before the call-embed
    * instruction (and this method) ever runs, so mutating some separate global
    * afterward can never reach it in time — only patching the argument value
    * itself, before it's ever sent, works. [[Interpreter.toHostFunc]]'s
    * different call site reuses this same method for the same reason: the
    * `state` value it's about to hand back to SpecTec is itself a plain local
    * value at that point, not a live reference either.
    */
  def pushMemoriesIntoStore(storeVal: Value): Value =
    toAL(st, storeVal) match
      case ALValue.StrV(fields) =>
        val patched = fields.map {
          case (name, ALValue.ListV(meminsts))
              if name.equalsIgnoreCase("MEMS") =>
            var result = meminsts
            for (memaddr, memoryObjAddr) <- memoryObjectCacheEntries() do
              arrayBufferDataAddr(memoryObjAddr).foreach { addr =>
                val i = memaddrIndex(memaddr)
                val bytes =
                  st(addr).asInstanceOf[ListObj].values.map(toALByte).toList
                result.lift(i).foreach {
                  case ALValue.StrV(miFields) =>
                    val patchedMi = ALValue.StrV(miFields.map {
                      case (fn, _) if fn.equalsIgnoreCase("BYTES") =>
                        (fn, ALValue.ListV(bytes))
                      case other => other
                    })
                    result = result.updated(i, patchedMi)
                  case _ => ()
                }
              }
            (name, ALValue.ListV(result))
          case other => other
        }
        Wasm(ALValue.StrV(patched))
      case _ =>
        storeVal // not a StrV store shape -- leave untouched (defensive)

  /** Applies freshly-pulled `bytes` (however they were obtained — an RPC result
    * or a slice of an already-in-hand store value) to `memoryObjAddr`'s
    * `ArrayBuffer`: an in-place refresh if the byte count matches the current
    * `ListObj`, or a real invocation of "refresh the Memory buffer" (via
    * [[invokeNamedWji]]) if it doesn't — grown, or never initialized. Matches
    * the real spec's own invocation discipline (that algorithm is only ever
    * invoked after an actual grow, never for a same-size content refresh),
    * rather than reimplementing its
    * `IsFixedLengthArrayBuffer`/`DetachArrayBuffer` branching natively here.
    */
  private def applyPulledBytes(
    memaddr: Value,
    memoryObjAddr: Value,
    bytes: List[ALValue],
    call: Call,
  ): Unit =
    arrayBufferDataAddr(memoryObjAddr) match
      case Some(addr)
          if st(addr).isInstanceOf[ListObj] &&
          st(addr).asInstanceOf[ListObj].values.length == bytes.length =>
        st(addr).asInstanceOf[ListObj].values = bytes.map(fromALNum).toVector
      case _ =>
        invokeNamedWji("refresh_the_memory_buffer", List(memaddr), call)

  /** wasm→JS: pull every live memory's current bytes straight out of an
    * already-in-hand `store` [[ALValue]] — the mirror image of
    * [[pushMemoriesIntoStore]]. Both call sites (`callEmbedding`'s
    * `func_invoke`/`module_instantiate` case, and [[Interpreter.toHostFunc]])
    * already have a `store` value in hand by the time they call this — no
    * separate RPC needed either way.
    */
  def pullMemoriesFromStore(storeAL: ALValue, call: Call): Unit =
    val mems: Option[List[ALValue]] = storeAL match
      case ALValue.StrV(fields) =>
        fields.collectFirst {
          case (name, ALValue.ListV(meminsts))
              if name.equalsIgnoreCase("MEMS") =>
            meminsts
        }
      case _ => None
    for
      meminsts <- mems
      (memaddr, memoryObjAddr) <- memoryObjectCacheEntries()
      mi <- meminsts.lift(memaddrIndex(memaddr))
    do
      mi match
        case ALValue.StrV(miFields) =>
          miFields
            .collectFirst {
              case (name, ALValue.ListV(bytes))
                  if name.equalsIgnoreCase("BYTES") =>
                bytes
            }
            .foreach(applyPulledBytes(memaddr, memoryObjAddr, _, call))
        case _ => ()
