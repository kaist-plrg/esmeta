package esmeta.wji.bridge.host

import esmeta.wji.state.ALValue

/** Errors that a [[WasmHost]] operation may produce. */
enum WasmError:
  /** the embedding function returned `error` (or, for `func_invoke`, threw a
    * Wasm exception) — mirrors AL's `error`/`EXCEPTION` results. The payload
    * is the AL value describing the error/exception. */
  case Trap(value: ALValue)
  /** transport/process-level failure: the spectec-server process died, sent
    * malformed JSON, called an unknown method, etc. */
  case ProtocolError(message: String)

/** Mirror of the subset of the Wasm Core Spec's Embedding interface
  * (`spectec/document/core/appendix/embedding.rst`) that is used by the
  * JS-API / WJI spec (`spectec/document/js-api/index.bs`). See
  * `core/EMBEDDING.md` for the full catalog and which functions are covered.
  *
  * Each method here corresponds 1:1 to an embedding function defined by the
  * Wasm standard (e.g. `module_decode`, `module_instantiate`, `func_invoke`)
  * and to a JSON-RPC method of the same name sent to `spectec-server`
  * (`wjmeta-bridge/spectec-server`). The full catalog, including operations
  * not yet covered here, is tracked in `core/EMBEDDING.md`.
  *
  * `wjmeta-mechanize` (the WJI mechanization) calls these methods when a
  * mechanized algorithm needs to perform a Wasm-level operation.
  * `wjmeta-bridge` provides the implementation, talking to the SpecTec
  * interpreter process. **The embedding functions themselves are not yet
  * implemented in SpecTec** — this trait is the interface SpecTec's
  * `spectec-server` is expected to expose.
  *
  * All parameters/results are [[ALValue]], SpecTec's own AL value
  * representation (see `core/value/ALValue.scala`), since that is the type
  * stores, modules, addresses, and `val*` are expressed in. Operations that
  * conceptually yield a composite result encode it as a single [[ALValue]]:
  * a pair such as `(store, funcaddr)` as [[ALValue.TupV]], a `val*` list as
  * [[ALValue.ListV]], a boolean as [[ALValue.BoolV]], and a unit / `error?`
  * success as [[ALValue.OptV]] `None`.
  *
  * Reentrancy: Wasm execution may call back into a wjmeta-side closure
  * registered via [[funcAlloc]] (see [[HostFunction]]). This happens over
  * the same JSON-RPC connection as a `host_func_invoke` request initiated by
  * `spectec-server`, handled on the wjmeta-bridge side.
  */
trait WasmHost:

  // -- Store --------------------------------------------------------------

  /** `store_init() : store` */
  def storeInit(): Either[WasmError, ALValue]

  // -- Modules --------------------------------------------------------------

  /** `module_decode(byte*) : module | error` */
  def moduleDecode(bytes: ALValue): Either[WasmError, ALValue]

  /** `module_validate(module) : error?` */
  def moduleValidate(module: ALValue): Either[WasmError, ALValue]

  /** `module_instantiate(store, module, externaddr*) : (store, moduleinst | exception | error)` */
  def moduleInstantiate(
    store: ALValue,
    module: ALValue,
    externVals: List[ALValue],
  ): Either[WasmError, ALValue]

  /** `module_imports(module) : (name, name, externtype)*` */
  def moduleImports(module: ALValue): Either[WasmError, ALValue]

  /** `module_exports(module) : (name, externtype)*` */
  def moduleExports(module: ALValue): Either[WasmError, ALValue]

  // -- Module instances -------------------------------------------------------

  /** `instance_export(moduleinst, name) : externaddr | error` */
  def instanceExport(moduleInst: ALValue, name: ALValue): Either[WasmError, ALValue]

  // -- Functions --------------------------------------------------------------

  /** `func_alloc(store, deftype, hostfunc) : (store, funcaddr)`
    *
    * `hostFunc` is registered by `wjmeta-bridge` so that SpecTec can call it
    * back during Wasm execution (see [[HostFunction]]).
    */
  def funcAlloc(
    store: ALValue,
    defType: ALValue,
    hostFunc: HostFunction,
  ): Either[WasmError, ALValue]

  /** `func_type(store, funcaddr) : deftype` */
  def funcType(store: ALValue, funcAddr: ALValue): Either[WasmError, ALValue]

  /** `func_invoke(store, funcaddr, val*) : (store, val* | exception | error)` */
  def funcInvoke(
    store: ALValue,
    funcAddr: ALValue,
    args: List[ALValue],
  ): Either[WasmError, ALValue]

  // -- Tables -----------------------------------------------------------------

  /** `table_alloc(store, tabletype, ref) : (store, tableaddr)` */
  def tableAlloc(store: ALValue, tableType: ALValue, ref: ALValue): Either[WasmError, ALValue]

  /** `table_type(store, tableaddr) : tabletype` */
  def tableType(store: ALValue, tableAddr: ALValue): Either[WasmError, ALValue]

  /** `table_read(store, tableaddr, i: u64) : ref | error` */
  def tableRead(store: ALValue, tableAddr: ALValue, i: ALValue): Either[WasmError, ALValue]

  /** `table_write(store, tableaddr, i: u64, ref) : store | error` */
  def tableWrite(store: ALValue, tableAddr: ALValue, i: ALValue, ref: ALValue): Either[WasmError, ALValue]

  /** `table_size(store, tableaddr) : u64` */
  def tableSize(store: ALValue, tableAddr: ALValue): Either[WasmError, ALValue]

  /** `table_grow(store, tableaddr, n: u64, ref) : store | error` */
  def tableGrow(store: ALValue, tableAddr: ALValue, n: ALValue, ref: ALValue): Either[WasmError, ALValue]

  // -- Memories -----------------------------------------------------------------

  /** `mem_alloc(store, memtype) : (store, memaddr)` */
  def memAlloc(store: ALValue, memType: ALValue): Either[WasmError, ALValue]

  /** `mem_type(store, memaddr) : memtype` */
  def memType(store: ALValue, memAddr: ALValue): Either[WasmError, ALValue]

  /** `mem_size(store, memaddr) : u64` */
  def memSize(store: ALValue, memAddr: ALValue): Either[WasmError, ALValue]

  /** `mem_grow(store, memaddr, n: u64) : store | error` */
  def memGrow(store: ALValue, memAddr: ALValue, n: ALValue): Either[WasmError, ALValue]

  // -- Tags -----------------------------------------------------------------

  /** `tag_alloc(store, tagtype) : (store, tagaddr)` */
  def tagAlloc(store: ALValue, tagType: ALValue): Either[WasmError, ALValue]

  /** `tag_type(store, tagaddr) : tagtype` */
  def tagType(store: ALValue, tagAddr: ALValue): Either[WasmError, ALValue]

  // -- Exceptions -----------------------------------------------------------------

  /** `exn_alloc(store, tagaddr, val*) : (store, exnaddr)` */
  def exnAlloc(store: ALValue, tagAddr: ALValue, vals: List[ALValue]): Either[WasmError, ALValue]

  /** `exn_tag(store, exnaddr) : tagaddr` */
  def exnTag(store: ALValue, exnAddr: ALValue): Either[WasmError, ALValue]

  /** `exn_read(store, exnaddr) : val*` */
  def exnRead(store: ALValue, exnAddr: ALValue): Either[WasmError, ALValue]

  // -- Globals -----------------------------------------------------------------

  /** `global_alloc(store, globaltype, val) : (store, globaladdr)` */
  def globalAlloc(store: ALValue, globalType: ALValue, value: ALValue): Either[WasmError, ALValue]

  /** `global_type(store, globaladdr) : globaltype` */
  def globalType(store: ALValue, globalAddr: ALValue): Either[WasmError, ALValue]

  /** `global_read(store, globaladdr) : val` */
  def globalRead(store: ALValue, globalAddr: ALValue): Either[WasmError, ALValue]

  /** `global_write(store, globaladdr, val) : store | error` */
  def globalWrite(store: ALValue, globalAddr: ALValue, value: ALValue): Either[WasmError, ALValue]

  // -- Values -----------------------------------------------------------------

  /** `ref_type(store, ref) : reftype` */
  def refType(store: ALValue, ref: ALValue): Either[WasmError, ALValue]

  /** `val_default(valtype) : val` */
  def valDefault(valType: ALValue): Either[WasmError, ALValue]

  // -- Matching -----------------------------------------------------------------

  /** `match_valtype(valtype, valtype) : bool` */
  def matchValType(valType1: ALValue, valType2: ALValue): Either[WasmError, ALValue]

  /** `match_externtype(externtype, externtype) : bool` */
  def matchExternType(externType1: ALValue, externType2: ALValue): Either[WasmError, ALValue]
