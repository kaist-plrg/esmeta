#!/usr/bin/env node

const { register_callback } = require("./repl");
const babel = require("./babel@7.19.1.min.js");

let transpile = (input) => {
  return babel.transform(input, {
    presets: ["env"],
    plugins: [
      ["transform-block-scoping", { tdz: true }],
    ],
    sourceType: "script",
    assumptions: {
      noDocumentAll: true,
      noNewArrows: false,
    }
  }).code;
}

register_callback(transpile, is_async = false)