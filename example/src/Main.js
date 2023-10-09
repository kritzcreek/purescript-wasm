import * as fs from 'node:fs'

export function writeToFileImpl(path, buf, cb) {
  fs.writeFile(path, buf, cb);
};

export function runWasmImpl(file) {
  fs.readFile(file, null, (err, buffer) => {
    WebAssembly.compile(buffer).then(module => {
      WebAssembly.instantiate(module, {}).then(inst => {
        console.log("Successfully created a Wasm module.")
        let res = inst.exports.main()
        console.log("Result:", res)
      })
    });
  });
};
