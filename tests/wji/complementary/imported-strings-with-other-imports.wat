(module
  (import "'" "hello" (global $s externref))
  (import "env" "n" (global $n i32))
  (func (export "s") (result externref) (global.get $s))
  (func (export "n") (result i32) (global.get $n)))
