#!/usr/bin/env node

const babel = require("./babel@7.19.1.min.js");
const fs = require("fs");
const path = require("path");

/** warn user about the usage */
let warnUsage = () => {
  console.log(`Usage: ${process.argv[1]} inputDir outputDir`);
  process.exit(0);
}

/** Do transpile using babel */
let transpile = (input) => {
  try {
    return babel.transform(input, {
      presets: ["env"],
      sourceType: "script",
      assumptions: {
        noDocumentAll: true
      }
    }).code;
  } catch {
    return 'throw "TRANSPILE_FAILURE";'
  }
}

let main = () => {
  const inputDir = process.argv[2];
  const outputDir = process.argv[3]

  if (inputDir === undefined || outputDir === undefined) {
    warnUsage();
  }

  fs.rmSync(outputDir, {recursive: true, force: true});
  fs.mkdirSync(outputDir, {recursive: true});

  for(let filename of fs.readdirSync(inputDir)) {
    let input = fs.readFileSync(path.join(inputDir, filename), "utf8")
    let output = transpile(input);
    fs.writeFileSync(path.join(outputDir, filename), output);
  }
}

main();
