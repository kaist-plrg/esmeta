(module
    (memory (export "memory") 1 4)

    (func (export "wasmGrow") (param $delta i32) (result i32)
        (memory.grow (local.get $delta)))

    (func (export "wasmWrite") (param $offset i32) (param $value i32)
        (i32.store (local.get $offset) (local.get $value)))

    (func (export "wasmRead") (param $offset i32) (result i32)
        (i32.load (local.get $offset)))
)
