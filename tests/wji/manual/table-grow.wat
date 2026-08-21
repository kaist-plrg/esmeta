(module
    (table (export "table") 1 4 funcref)

    (func $f (result i32) (i32.const 7))
    (func (export "g") (result i32) (i32.const 8))
    (elem (i32.const 0) func $f)

    (func (export "wasmGrowTable") (param $delta i32) (result i32)
        (table.grow (ref.null func) (local.get $delta)))
)
