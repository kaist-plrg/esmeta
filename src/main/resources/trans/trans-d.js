const fs = require("fs");
const path = require("path");
  
const inputDir = process.argv[2];
const outputDir = process.argv[3];
const onlyListFile = process.argv[4];
  
const onlyList = onlyListFile ? fs.readFileSync(onlyListFile, "utf8").split("\n") : undefined;
const onlySet = onlyList ? new Set(onlyList) : undefined;
const skip = (filename) => {
  if(!onlySet) return false
  return !onlySet.has(filename);
}

/** warn user about the usage */
let warnUsage = () => {
  console.log(`Usage: ${process.argv[1]} inputDir outputDir`);
  process.exit(1);
}

/** Transpile directory using a given transpile fucntion */
let transpileUsing = (transpile) => {
  if (inputDir === undefined || outputDir === undefined) {
    warnUsage();
  }

  fs.rmSync(outputDir, {recursive: true, force: true});
  fs.mkdirSync(outputDir, {recursive: true});

  for(let filename of fs.readdirSync(inputDir)) {
    if(skip(filename)) continue;
    let input = fs.readFileSync(path.join(inputDir, filename), "utf8")
    let output;
    try {
      output = transpile(input);
    } catch (e) {
      output = `throw "TRANSPILE_FAILURE";\n/* ${e} */\n`;
    }
    fs.writeFileSync(path.join(outputDir, filename), output);
  }
}

/** Transpile directory asynchronously using a given transpile fucntion */
let transpileAsyncUsing = async (transpileAsync) => {
  if (inputDir === undefined || outputDir === undefined) {
    warnUsage();
  }

  fs.rmSync(outputDir, {recursive: true, force: true});
  fs.mkdirSync(outputDir, {recursive: true});

  for(let filename of fs.readdirSync(inputDir)) {
    if(skip(filename)) continue;
    let input = fs.readFileSync(path.join(inputDir, filename), "utf8")
    let output;
    try {
      output = await transpileAsync(input);
    } catch (e) {
      output = `throw "TRANSPILE_FAILURE";\n/* ${e} */\n`;
    }
    fs.writeFileSync(path.join(outputDir, filename), output);
  }
}

module.exports = { transpileUsing, transpileAsyncUsing };
