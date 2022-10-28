#!/usr/bin/env node

const terser = require("/usr/local/lib/node_modules/terser");
const { transpileAsyncUsing } = require("./trans-d.js");

/** Do transpile using terser */
let transpileAsync = async (input) => {
  return (await terser.minify(input, {
    mangle: false,
  })).code;
}

transpileAsyncUsing(transpileAsync);
