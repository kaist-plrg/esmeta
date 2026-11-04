package esmeta.interpreter

import esmeta.error.WasmHostFailure
import esmeta.ir.GLOBAL_AGENT_RECORD
import esmeta.state.*
import esmeta.state.util.{fromALNum, toAL}

/** Syncs JS `ArrayBuffer` bytes with a wasm store's linear memory across the
  * ESMeta (JVM) / SpecTec (separate OCaml process) boundary. True aliasing —
  * what js-api's own "identified with" phrasing assumes — is architecturally
  * impossible here, since the two only ever talk over JSON-RPC; instead, the
  * bridge keeps one invariant: whenever SpecTec acts on a store, the store
  * holds every byte JS has written. JS-side bytes are pushed into the `store`
  * argument of every embedding call that takes one
  * ([[Interpreter.callEmbedding]]; not only `func_invoke`/`module_instantiate`
  * -- e.g. a JS write followed by `Memory.prototype.grow` would otherwise be
  * lost by `mem_grow`), and pulled back after `func_invoke`/
  * `module_instantiate` and around [[Interpreter.toHostFunc]] (wasm reentrantly
  * calls back into JS). These call sites have an explicit `store` value in hand
  * (`create a host function`'s closure was `SpecPatch`-corrected to take/
  * return `state` explicitly, same as every other store-touching js-api
  * algorithm — see `docs/spec_inconsistencies.md` #7), so all of them go
  * through the same pure-local, RPC-free pair below: [[pushMemoriesIntoStore]]/
  * [[pullMemoriesFromStore]]. `mem_read_bytes`, which only builds a new
  * buffer's data block, reads SpecTec's live store instead, so it prefers an
  * attached same-length JS buffer ([[currentJsBytes]]).
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
    val cacheAddr = st(agentRecordAddr, Str("memory object cache")).asAddr
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

  private def memaddrIndex(memaddr: Value): Int = toAL(st, memaddr) match
    case ALValue.NumV(ALNum.Nat(n)) => n.toInt
    case ALValue.NumV(ALNum.Int(n)) => n.toInt
    case other => throw WasmHostFailure(s"expected a nat memaddr, got $other")

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
        var changed = false
        val patched = fields.map {
          case (name, ALValue.ListV(meminsts))
              if name.equalsIgnoreCase("MEMS") =>
            var result = meminsts
            for
              (memaddr, memoryObjAddr) <- memoryObjectCacheEntries()
              js <- jsBytes(memoryObjAddr)
              i = memaddrIndex(memaddr)
              mi <- result.lift(i)
            do
              mi match
                case ALValue.StrV(miFields) =>
                  val patchedMi = ALValue.StrV(miFields.map {
                    case (fn, ALValue.ListV(old))
                        if fn.equalsIgnoreCase("BYTES") &&
                        old.length != js.length =>
                      throw resizedWithoutRefresh(
                        memaddr,
                        old.length,
                        Some(js.length),
                      )
                    case (fn, ALValue.ListV(old))
                        if fn.equalsIgnoreCase("BYTES") &&
                        !old.iterator.zip(js.iterator).forall(_ == _) =>
                      changed = true
                      (fn, ALValue.ListV(js.toList))
                    case other => other
                  })
                  result = result.updated(i, patchedMi)
                case _ => ()
            (name, ALValue.ListV(result))
          case other => other
        }
        // keep the very same value when nothing changed, so that
        // `SpecTecWasmHost.encodeStore` can still send just its cached id
        if changed then Wasm(ALValue.StrV(patched)) else storeVal
      case _ =>
        storeVal // not a StrV store shape -- leave untouched (defensive)

  /** `memoryObjAddr`'s current JS-side bytes as AL values, or `None` if its
    * buffer is detached.
    */
  private def jsBytes(memoryObjAddr: Value): Option[Vector[ALValue]] =
    arrayBufferDataAddr(memoryObjAddr).map(addr =>
      st(addr).asInstanceOf[ListObj].values.map(toAL(st, _)),
    )

  /** the current JS-side bytes of the memory at `memaddr`, if its buffer is
    * attached and holds exactly `length` bytes. `mem_read_bytes` reads
    * SpecTec's live store, which has not seen JS writes made since the last
    * push; a same-length attached buffer is always at least as new.
    */
  def currentJsBytes(memaddr: Value, length: Int): Option[Vector[Value]] =
    val i = memaddrIndex(memaddr)
    memoryObjectCacheEntries()
      .collectFirst {
        case (addr, memoryObjAddr) if memaddrIndex(addr) == i => memoryObjAddr
      }
      .flatMap(arrayBufferDataAddr)
      .map(st(_))
      .collect {
        case list: ListObj if list.values.length == length => list.values
      }

  /** Copies freshly-pulled `bytes` into `memoryObjAddr`'s `ArrayBuffer` in
    * place.
    */
  private def applyPulledBytes(
    memaddr: Value,
    memoryObjAddr: Value,
    bytes: List[ALValue],
  ): Unit =
    arrayBufferDataAddr(memoryObjAddr).map(st(_)) match
      case Some(list: ListObj) if list.values.length == bytes.length =>
        list.values = bytes.map(fromALNum).toVector
      case other =>
        val bufferLength = other.collect {
          case list: ListObj =>
            list.values.length
        }
        throw resizedWithoutRefresh(memaddr, bytes.length, bufferLength)

  /** Every size change refreshes the buffer right away, either in
    * `Memory.prototype.grow` itself or in the `memory.grow` hook
    * ([[Interpreter.memoryGrowHook]]), so the store and the JS buffer always
    * agree on a memory's length; a mismatch means some path skipped both.
    */
  private def resizedWithoutRefresh(
    memaddr: Value,
    storeLength: Int,
    bufferLength: Option[Int],
  ): WasmHostFailure = WasmHostFailure(
    s"memory $memaddr resized without refreshing its buffer: " +
    s"$storeLength bytes in the store, " +
    bufferLength.fold("detached buffer")(n => s"$n bytes in the buffer"),
  )

  /** wasm→JS: pull every live memory's current bytes straight out of an
    * already-in-hand `store` [[ALValue]] — the mirror image of
    * [[pushMemoriesIntoStore]]. Both call sites (`callEmbedding`'s
    * `func_invoke`/`module_instantiate` case, and [[Interpreter.toHostFunc]])
    * already have a `store` value in hand by the time they call this — no
    * separate RPC needed either way.
    */
  def pullMemoriesFromStore(storeAL: ALValue): Unit =
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
            .foreach(applyPulledBytes(memaddr, memoryObjAddr, _))
        case _ => ()
