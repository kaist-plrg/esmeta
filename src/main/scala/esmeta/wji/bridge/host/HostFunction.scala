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
  * `embedding.rst`'s own `embed-invoke-host` contract is `val* -> val* |
  * exception | error` (no store in or out) — but that's the *unpatched* js-api
  * spec's gap this project fixes: `create a host function` (js-api/index.bs) is
  * patched (`SpecPatch` #28/#29/#34) to take `state` explicitly and return
  * `(state, instr*)` as a pair, matching every other store-touching algorithm
  * in that document, so this type is WJI's own *corrected* version — `(ALValue,
  * List[ALValue]) => Either[WasmError, (ALValue, List[ALValue])]` — store
  * genuinely threaded in and back out, not the store-less shape the unpatched
  * spec text would suggest. The second `List[ALValue]` is `instr*` verbatim —
  * the host function's own return value(s) on the ordinary path, or the
  * `(ref.exn a) throw_ref` pair on the path where it re-throws a JS exception
  * across the Wasm call boundary (`$callhostfunc`'s own doc,
  * `4.3-execution.instructions.spectec`, has the full reasoning for why this is
  * `instr*` rather than the spec's own `result` syntax) — this type doesn't
  * distinguish the two, any more than the spectec-side plumbing it mirrors
  * does.
  *
  * `wjmeta-mechanize` provides implementations (running the mechanized WJI IR
  * interpreter); `wjmeta-bridge` registers them with [[WasmHost.funcAlloc]] and
  * dispatches incoming `host_func_invoke` requests from SpecTec to them.
  */
type HostFunction =
  (ALValue, List[ALValue]) => Either[WasmError, (ALValue, List[ALValue])]
