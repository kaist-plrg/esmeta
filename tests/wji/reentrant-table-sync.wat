(module
    (import "env" "callback" (func $callback))
    ;; see table-mutation.wat: re-exporting an import is how JS gets a
    ;; funcref-legal handle to its own function, to hand back via table.set.
    (import "env" "postFn" (func $postFn (result i32)))
    (export "postFnHandle" (func $postFn))

    (table (export "table") 2 2 funcref)

    (type $sig (func (result i32)))

    (func $preFn (result i32) (i32.const 55))
    (elem declare func $preFn)

    (func (export "run") (result i32)
        (table.set (i32.const 0) (ref.func $preFn))
        call $callback
        (call_indirect (type $sig) (i32.const 1)))
)
