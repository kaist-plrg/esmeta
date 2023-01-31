const fs = require("fs");
const path = require("path");

const inputFile = process.argv[2];
const outputFile = process.argv[4];
const onlyListFile = process.argv[5];

const onlyList = onlyListFile ? fs.readFileSync(onlyListFile, "utf8").split("\n") : undefined;
const onlySet = onlyList ? new Set(onlyList) : undefined;
const skip = (filename) => {
  if(!onlySet) return false
  return !onlySet.has(filename);
}
/** warn user about the usage */
let warnUsage = () => {
  console.log(`Usage: ${process.argv[1]} inputFile -o outputFile`);
  process.exit(1);
}

/** Transpile file using a given transpile fucntion */
let transpileUsing = (transpile) => {
  if (inputFile === undefined || outputFile === undefined) {
    warnUsage();
  }

  let input = fs.readFileSync(inputFile, "utf8")
  let output;
  try {
    output = transpile(input);
  } catch (e) {
    output = `throw "TRANSPILE_FAILURE";\n/* ${e} */\n`;
  }
  fs.writeFileSync(outputFile, output);
}

/** Transpile directory asynchronously using a given transpile fucntion */
let transpileAsyncUsing = async (transpileAsync) => {
  if (inputFile === undefined || outputFile === undefined) {
    warnUsage();
  }

  let input = fs.readFileSync(inputFile, "utf8")
  let output;
  try {
    output = transpile(input);
  } catch (e) {
    output = `throw "TRANSPILE_FAILURE";\n/* ${e} */\n`;
  }
  fs.writeFileSync(outputFile, output);
}

module.exports = { transpileUsing, transpileAsyncUsing };
