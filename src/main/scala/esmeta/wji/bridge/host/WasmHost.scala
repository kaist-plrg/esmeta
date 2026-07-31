package esmeta.wji.bridge.host

import esmeta.state.ALValue

/** Errors that a [[WasmHost]] operation may produce. */
enum WasmError:
  /** the embedding function returned `error` (or, for `func_invoke`, threw a
    * Wasm exception) — mirrors AL's `error`/`EXCEPTION` results. The payload is
    * the AL value describing the error/exception.
    */
  case Trap(value: ALValue)

  /** transport/process-level failure: the spectec-server process died, sent
    * malformed JSON, called an unknown method, etc.
    */
  case ProtocolError(message: String)

/** Mirror of the subset of the Wasm Core Spec's Embedding interface
  * (`spectec/document/core/appendix/embedding.rst`) that is used by the JS-API
  * / WJI spec (`spectec/document/js-api/index.bs`). See `core/EMBEDDING.md` for
  * the full catalog and which functions are covered.
  *
  * Every embedding function here corresponds 1:1 to a JSON-RPC method of the
  * same name sent to `spectec-server` (`wjmeta-bridge/spectec-server`), and
  * (except [[WasmHost.funcAlloc]]) is a pure "encode positional AL args as a
  * named JSON object, send, decode the result" round trip with no logic of its
  * own — so rather than one Scala method per function (many near-identical
  * bodies, each just re-stating its own name three times: the `names` entry,
  * the method name, and the JSON-RPC method string), every one of them is
  * dispatched generically through [[WasmHost.call]], keyed by the same
  * `paramNames` table that also defines `names`.
  *
  * `wjmeta-mechanize` (the WJI mechanization) calls these functions when a
  * mechanized algorithm needs to perform a Wasm-level operation.
  * `wjmeta-bridge` provides the implementation, talking to the SpecTec
  * interpreter process. **Not every embedding function is implemented in
  * SpecTec yet** — calling one that isn't produces a
  * [[WasmError.ProtocolError]] from the RPC round trip itself (`spectec-server`
  * responds "method not implemented"), rather than a Scala-side `???`.
  *
  * All parameters/results are [[ALValue]], SpecTec's own AL value
  * representation (see `core/value/ALValue.scala`), since that is the type
  * stores, modules, addresses, and `val*` are expressed in. Operations that
  * conceptually yield a composite result encode it as a single [[ALValue]]: a
  * pair such as `(store, funcaddr)` as [[ALValue.TupV]], a `val*` list as
  * [[ALValue.ListV]], a boolean as [[ALValue.BoolV]], and a unit / `error?`
  * success as [[ALValue.OptV]] `None`.
  *
  * Reentrancy: Wasm execution may call back into a wjmeta-side closure
  * registered via [[WasmHost.funcAlloc]] (see [[HostFunction]]). This happens
  * over the same JSON-RPC connection as a `host_func_invoke` request initiated
  * by `spectec-server`, handled on the wjmeta-bridge side.
  */
object WasmHost:

  /** Ordered JSON-RPC parameter names for every embedding function *except*
    * `func_alloc` (dispatched separately by [[WasmHost.funcAlloc]], since —
    * unlike everything else here — it must additionally register a reentrant
    * [[HostFunction]] before making the RPC call, not just forward its
    * arguments). `esmeta.interpreter.Interpreter.callEmbedding` converts every
    * argument uniformly via `toAL` (a heap-allocated `val*`/`externval*` list
    * argument, e.g. `func_invoke`'s `args`, is recursively converted to an
    * `ALValue.ListV` there too — see its doc), so no per-parameter "is this a
    * list" bookkeeping is needed here, just names. The comment above each entry
    * is the function's Embedding-API signature, mirroring `embedding.rst`.
    */
  val paramNames: Map[String, List[String]] = Map(
    // store_init() : store
    "store_init" -> Nil,
    // module_decode(byte*) : module | error
    "module_decode" -> List("bytes"),
    // module_validate(module) : error?
    "module_validate" -> List("module"),
    // module_instantiate(store, module, externaddr*) : (store, moduleinst | exception | error)
    "module_instantiate" -> List("store", "module", "externvals"),
    // module_imports(module) : (name, name, externtype)*
    "module_imports" -> List("module"),
    // module_exports(module) : (name, externtype)*
    "module_exports" -> List("module"),
    // expand(deftype) : comptype — not part of the Wasm Core Spec's own
    // Embedding API; a wjmeta-bridge-specific convenience wrapping the
    // $Expand relation (see spectec-server's Embedding.expand), needed
    // because js-api/index.bs destructures a deftype's underlying comptype
    // directly without ever calling out to $expand itself (docs/spec_errors.md).
    "expand" -> List("deftype"),
    // instance_export(moduleinst, name) : externaddr | error
    "instance_export" -> List("moduleinst", "name"),
    // func_type(store, funcaddr) : deftype
    "func_type" -> List("store", "funcaddr"),
    // func_invoke(store, funcaddr, val*) : (store, val* | exception | error)
    "func_invoke" -> List("store", "funcaddr", "args"),
    // table_alloc(store, tabletype, ref) : (store, tableaddr)
    "table_alloc" -> List("store", "tabletype", "ref"),
    // table_type(store, tableaddr) : tabletype
    "table_type" -> List("store", "tableaddr"),
    // table_read(store, tableaddr, i: u64) : ref | error
    "table_read" -> List("store", "tableaddr", "i"),
    // table_write(store, tableaddr, i: u64, ref) : store | error
    "table_write" -> List("store", "tableaddr", "i", "ref"),
    // table_size(store, tableaddr) : u64
    "table_size" -> List("store", "tableaddr"),
    // table_grow(store, tableaddr, n: u64, ref) : store | error
    "table_grow" -> List("store", "tableaddr", "n", "ref"),
    // mem_alloc(store, memtype) : (store, memaddr)
    "mem_alloc" -> List("store", "memtype"),
    // mem_type(store, memaddr) : memtype
    "mem_type" -> List("store", "memaddr"),
    // mem_size(store, memaddr) : u64
    "mem_size" -> List("store", "memaddr"),
    // mem_grow(store, memaddr, n: u64) : store | error
    "mem_grow" -> List("store", "memaddr", "n"),
    // mem_read_bytes(memaddr) : byte* -- not part of embedding.rst (mem_read
    // is byte-at-a-time and unused by js-api itself); a wjmeta-bridge-
    // specific bulk read, used by ExpandDataBlockOfPass (the "a Data Block
    // which is identified with the underlying memory of |memaddr|" idiom in
    // create_a_fixed_length_memory_buffer/refresh_the_memory_buffer, see
    // docs/hardcodes.md). Deliberately implicit-store (unlike every other
    // entry here) -- always reads whatever SpecTec's live store currently
    // holds, no separate "caller's store" argument needed. See
    // embedding.ml's own doc comment on this function for the full
    // reasoning.
    "mem_read_bytes" -> List("memaddr"),
    // tag_alloc(store, tagtype) : (store, tagaddr)
    "tag_alloc" -> List("store", "tagtype"),
    // tag_type(store, tagaddr) : tagtype
    "tag_type" -> List("store", "tagaddr"),
    // exn_alloc(store, tagaddr, val*) : (store, exnaddr)
    "exn_alloc" -> List("store", "tagaddr", "vals"),
    // exn_tag(store, exnaddr) : tagaddr
    "exn_tag" -> List("store", "exnaddr"),
    // exn_read(store, exnaddr) : val*
    "exn_read" -> List("store", "exnaddr"),
    // global_alloc(store, globaltype, val) : (store, globaladdr)
    "global_alloc" -> List("store", "globaltype", "value"),
    // global_type(store, globaladdr) : globaltype
    "global_type" -> List("store", "globaladdr"),
    // global_read(store, globaladdr) : val
    "global_read" -> List("store", "globaladdr"),
    // global_write(store, globaladdr, val) : store | error
    "global_write" -> List("store", "globaladdr", "value"),
    // ref_type(store, ref) : reftype
    "ref_type" -> List("store", "ref"),
    // val_default(valtype) : val
    "val_default" -> List("valtype"),
    // match_valtype(valtype, valtype) : bool
    "match_valtype" -> List("valtype1", "valtype2"),
    // match_externtype(externtype, externtype) : bool
    "match_externtype" -> List("externtype1", "externtype2"),
    // Not part of the formal embedding.rst boundary -- these are Wasm Core
    // spec numerics (signed_(N)) that js-api prose calls directly by their
    // rendered per-width names. The server translates the name back to the
    // real parametric `signed` op; see `server.ml`'s `call_signed`.
    // signed_31(i: u31) : i31 (interpreted as a mathematical value)
    "signed_31" -> List("i"),
    // signed_32(i: u32) : i32 (interpreted as a mathematical value)
    "signed_32" -> List("i"),
    // signed_64(i: u64) : i64 (interpreted as a mathematical value)
    "signed_64" -> List("i"),
    // Not spec-link text at all -- WJI's own name for `signed_N`'s inverse
    // (js-api prose never names it directly, only describes it via "the
    // unsigned integer such that |i| is [=signed_N=](|u|)";
    // `ExpandExistentialsPass` is what recognizes that idiom and emits this call).
    // Same per-width/server-translates-to-parametric split as signed_N above;
    // see server.ml's `call_inv_signed`.
    // inv_signed_31(i: i31) : u31
    "inv_signed_31" -> List("i"),
    // inv_signed_32(i: i32) : u32
    "inv_signed_32" -> List("i"),
    // inv_signed_64(i: i64) : u64
    "inv_signed_64" -> List("i"),
  )

  /** The embedding function names this trait implements (`paramNames`'s keys,
    * plus `func_alloc`, dispatched separately). Used by
    * `esmeta.wji.compiler.Compiler` to recognize a `[=...=]` link as an
    * embedding call (compiled to `ir.ICallEmbed`) rather than a WJI/ES function
    * call (`ir.ICall`).
    */
  val names: Set[String] = paramNames.keySet + "func_alloc"

trait WasmHost:

  /** Calls the embedding function `name` (a key of [[WasmHost.paramNames]])
    * with `args`, positional in the order [[WasmHost.paramNames]]`(name)`
    * declares. Every embedding function except `func_alloc` is dispatched
    * through this single method — see [[WasmHost.funcAlloc]] for why that one
    * stays separate.
    */
  def call(name: String, args: List[ALValue]): Either[WasmError, ALValue]

  /** `func_alloc(store, deftype, hostfunc) : (store, funcaddr)`
    *
    * `hostFunc` is registered by `wjmeta-bridge` so that SpecTec can call it
    * back during Wasm execution (see [[HostFunction]]) — the one embedding
    * function that needs to do more than forward its arguments, so it keeps its
    * own method rather than going through [[call]].
    */
  def funcAlloc(
    store: ALValue,
    defType: ALValue,
    hostFunc: HostFunction,
  ): Either[WasmError, ALValue]
