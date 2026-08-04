;; js-throw-propagation.wat: exports callImport(), which calls an imported
;; JS function `cb`. Checks that a JS exception thrown inside `cb` propagates
;; back to the original JS caller unwrapped -- the same object, not a generic
;; WebAssembly.RuntimeError -- crossing the wasm frame in between.
(module
  (import "env" "cb" (func $cb))
  (func (export "callImport")
    call $cb)
)
