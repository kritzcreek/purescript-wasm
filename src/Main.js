const fs = require('fs');

exports.writeToFileImpl = function(path, buf, cb) {
  fs.writeFile(path, buf, cb);
};

exports.runWasm = (file) => () => {
  fs.readFile(file, null, (err, buffer) => {
    WebAssembly.compile(buffer).then(module => {
      WebAssembly.instantiate(module, {}).then(inst => {
        console.log("Succesfully created a Wasm module.")
        console.log(inst.exports)
      })
    });
  });
};
