import * as fs from 'node:fs'

export function writeToFileImpl(path, buf, cb) {
  fs.writeFile(path, buf, cb);
};

export const runWasm = (file) => () => {
  fs.readFile(file, null, (err, buffer) => {
    WebAssembly.compile(buffer).then(module => {
      WebAssembly.instantiate(module, {}).then(inst => {
        console.log("Succesfully created a Wasm module.")
        console.log(inst.exports)
        let res = inst.exports.main()
        console.log("Result: ", res)
      })
    });
  });
};
