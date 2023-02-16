#!/usr/bin/env node

const { register_callback } = require("./repl");
const swc = require("./node_modules/@swc/core");

let transpile = (input) => {
  return swc.transformSync(input, {
    isModule: false,
  }).code;
}

register_callback(transpile, is_async = false)
