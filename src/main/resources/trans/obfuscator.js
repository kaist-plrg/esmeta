#!/usr/bin/env node

const { register_callback } = require("./repl");
const { obfuscate } = require("/usr/local/lib/node_modules/javascript-obfuscator");

let transpile = (input) => {
  return obfuscate(input, {
    seed: 1
  }).getObfuscatedCode();
}

register_callback(transpile, is_async = false)
