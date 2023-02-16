#!/usr/bin/env node

const { register_callback } = require("./repl");
const terser = require("./node_modules/terser");

let transpile = async (input) => {
  return (await terser.minify(input, {
    mangle: false,
    keep_fnames: true,
    keep_classnames: true,
  })).code;
}

register_callback(transpile, is_async = true)
