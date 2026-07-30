;; js-throw-through-wasm.wat: exports callImport(), which calls an imported
;; JS function `cb`. Used to check whether a JS exception thrown inside `cb`
;; propagates back to the original JS caller unwrapped (real engines do this),
;; or gets swallowed into a generic WebAssembly.RuntimeError (a known WJI gap
;; -- see docs/hardcodes.md #9, SpecPatch #23).
(module
  (import "env" "cb" (func $cb))
  (func (export "callImport")
    call $cb)
)
