(module
    (import "env" "callback" (func $callback))
    (global $g (export "g") (mut i32) (i32.const 0))

    (func (export "run") (param $preVal i32) (result i32)
        local.get $preVal
        global.set $g
        call $callback
        global.get $g)
)
