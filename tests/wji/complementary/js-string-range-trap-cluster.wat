(module
  ;; final (the text format's default), as the js-string builtins require
  (type $arr (array (mut i16)))
  (import "wasm:js-string" "charCodeAt"
    (func $charCodeAt (param externref i32) (result i32)))
  (import "wasm:js-string" "fromCharCodeArray"
    (func $fromCCA (param (ref null $arr) i32 i32) (result (ref extern))))
  (import "wasm:js-string" "intoCharCodeArray"
    (func $intoCCA (param externref (ref null $arr) i32) (result i32)))
  (func (export "charCodeAt") (param $s externref) (param $i i32) (result i32)
    (call $charCodeAt (local.get $s) (local.get $i)))
  (func (export "fromCharCodeArray")
    (param $len i32) (param $start i32) (param $end i32) (result (ref extern))
    (call $fromCCA (array.new_default $arr (local.get $len))
                   (local.get $start) (local.get $end)))
  (func (export "intoCharCodeArray")
    (param $s externref) (param $len i32) (param $start i32) (result i32)
    (call $intoCCA (local.get $s) (array.new_default $arr (local.get $len))
                   (local.get $start))))
