(module
  (import "m" "t" (tag $imported (param i32)))
  (tag $exported (param i32))
  (export "t" (tag $exported))
)
