(module
  (import "env" "v128Result" (func $v128Result (result v128)))
  (import "env" "pair" (func $pair (result i32 i32)))
  (func (export "callV128") (call $v128Result) (drop))
  (func (export "callPair") (call $pair) (drop) (drop)))
