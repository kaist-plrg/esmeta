(module
  (func (export "returnsV128") (result v128) (v128.const i64x2 0 0))
  (func (export "addOne") (param i32) (result i32) (i32.add (local.get 0) (i32.const 1))))
