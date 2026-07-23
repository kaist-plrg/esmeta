(module
    (global $g (export "g") (mut i32) (i32.const 10))

    (func (export "wasmSetGlobal") (param $value i32)
        (global.set $g (local.get $value)))

    (func (export "wasmGetGlobal") (result i32)
        (global.get $g))
)
