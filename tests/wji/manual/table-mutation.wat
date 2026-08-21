(module
    ;; ToWebAssemblyValue for funcref requires the value be null or already
    ;; carry a [[FunctionAddress]] slot -- a plain JS closure doesn't qualify,
    ;; so the JS side needs a handle to a wasm-wrapped version of its own
    ;; function. Importing it and re-exporting the same func index gives JS
    ;; exactly that handle.
    (import "env" "jsFn" (func $jsFn (result i32)))
    (export "jsFnHandle" (func $jsFn))

    (table (export "table") 2 2 funcref)

    (func $wasmFn (result i32) (i32.const 42))
    (elem (i32.const 0) func $wasmFn)

    (type $sig (func (result i32)))
    (func (export "wasmCallTable") (param $index i32) (result i32)
        (call_indirect (type $sig) (local.get $index)))
)
