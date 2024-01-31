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
  const swc = require(join(stdout.toString().trim(), "@swc/core"));
  const { transpileUsing } = require("./trans-d.js");

  /** Do transpile using swc */
  let transpile = (input) => {
    return swc.transformSync(input, {
      isModule: false,
    }).code;
  }

  transpileUsing(transpile);
});