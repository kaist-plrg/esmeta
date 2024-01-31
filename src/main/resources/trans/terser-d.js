#!/usr/bin/env node

const { exec } = require("child_process");
const { join } = require("path");
exec("npm root -g", (error, stdout, stderr) => {
  if (error) {
    throw new Error("The command `npm root -g` didn't work.");
  }
  if (stderr) {
    throw new Error("The command `npm root -g` didn't work.");
  }
  const terser = require(join(stdout.toString().trim(), "terser"));
  const { transpileAsyncUsing } = require("./trans-d.js");

  /** Do transpile using terser */
  let transpileAsync = async (input) => {
    return (await terser.minify(input, {
      mangle: false,
      keep_fnames: true,
      keep_classnames: true,
    })).code;
  }

  transpileAsyncUsing(transpileAsync);
});