(module
  (func (export "posInf32") (result f32) (f32.const inf))
  (func (export "negInf32") (result f32) (f32.const -inf))
  (func (export "nan32") (result f32) (f32.const nan))
  (func (export "posInf64") (result f64) (f64.const inf))
  (func (export "negInf64") (result f64) (f64.const -inf))
  (func (export "nan64") (result f64) (f64.const -nan:0x1))
  (func (export "bits32") (param f32) (result i32) (i32.reinterpret_f32 (local.get 0)))
  (func (export "bits64") (param f64) (result i64) (i64.reinterpret_f64 (local.get 0))))
