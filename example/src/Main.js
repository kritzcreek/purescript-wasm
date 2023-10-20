import * as fs from 'node:fs'
import * as cp from 'node:child_process'

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


export function printWasmImpl(file) {
  let res = cp.spawnSync("wasm-tools", ["print", file]);
  console.log("Out\n", res.stdout.toString())
  if (res.stderr.toString() !== "") {
    console.log("Err\n", res.stderr.toString())
  }
}
