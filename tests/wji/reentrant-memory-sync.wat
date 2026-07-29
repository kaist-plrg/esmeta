(module
    (import "env" "callback" (func $callback))
    (memory (export "memory") 1)
    (func (export "run")
        (param $preOff i32) (param $preVal i32) (param $postOff i32)
        (result i32)
        local.get $preOff
        local.get $preVal
        i32.store
        call $callback
        local.get $postOff
        i32.load)
)
