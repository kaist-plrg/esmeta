;; wasm-throw-propagation.wat: exports tag `t` (single i32 param) and
;; throwIt(i32), which unconditionally throws it. Checks that a wasm-native
;; exception (the `throw` instruction, uncaught inside wasm) surfaces to JS as
;; a WebAssembly.Exception carrying the tag identity and payload -- distinct
;; from both a trap (wasm-trap-propagation.js) and a JS exception simply
;; passing through a wasm frame (js-throw-propagation.js).
(module
  (tag $t (export "t") (param i32))
  (func (export "throwIt") (param i32)
    local.get 0
    throw $t)
)
