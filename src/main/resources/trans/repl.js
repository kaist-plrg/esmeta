const readline = require("readline");

const rl = readline.createInterface({
	input: process.stdin,
	output: process.stdout,
});

let register_callback = (transpile, is_async) => {
  rl.on("line", (line) => {
    let code = '"use strict";\n' + line;
    if(is_async) {
      transpile(code)
      .then(transpiled => {
        console.log(transpiled);
      })
      .catch(e => {
        console.log(`throw "TRANSPILE_FAILURE";\n/*\n${e}\n*/`);
      })
      .finally(() => {
        console.log("DONE");
      })
    }
    else {
      try {
        console.log(transpile(code));
      } catch (e) {
        console.log(`throw "TRANSPILE_FAILURE";\n/*\n${e}\n*/`);
      }
      console.log("DONE");
    }
  });
}

module.exports = { register_callback };
