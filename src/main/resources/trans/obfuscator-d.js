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
  const { obfuscate } = require(join(stdout.toString().trim(), "javascript-obfuscator"));
  const { transpileUsing } = require("./trans-d.js");

  /** Do obfuscate */
  let transpile = (input) => {
    return obfuscate(input, {
      seed: 1
    }).getObfuscatedCode();
  }

  transpileUsing(transpile);
});