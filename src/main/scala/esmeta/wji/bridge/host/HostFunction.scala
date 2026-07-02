package esmeta.wji.bridge.host

import esmeta.state.ALValue

/** A wjmeta-side (WJI mechanization) closure that SpecTec calls back into while
  * executing Wasm code, corresponding to the `hostfunc` argument of
  * `func_alloc` (`embed-func-alloc` in
  * `spectec/document/core/appendix/embedding.rst`):
  *
  * {{{
  * func_alloc(store, deftype, hostfunc) : (store, funcaddr)
  * }}}
  *
  * The pre-/post-condition of `hostfunc` itself is `val* -> val* | exception
  * | error` (see `embed-invoke-host` in the same document), which this type
  * mirrors as `List[ALValue] => Either[WasmError, List[ALValue]]`.
  *
  * `wjmeta-mechanize` provides implementations (running the mechanized WJI IR
  * interpreter); `wjmeta-bridge` registers them with [[WasmHost.funcAlloc]] and
  * dispatches incoming `host_func_invoke` requests from SpecTec to them.
  */
type HostFunction = List[ALValue] => Either[WasmError, List[ALValue]]
