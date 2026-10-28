(module
  (import "env" "n" (global $n i32))
  (func (export "n") (result i32) (global.get $n)))
