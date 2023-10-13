purescript-wasm
===

A PureScript library containing syntax definitions and encoders for WebAssembly. You can use this to generate Wasm for your own compiler projects. This library does *not* compile PureScript to Wasm.

## Minimal example

As a small example we'll define a module with a `main` function that takes no arguments and returns an i32. It'll add the numbers 20 and 22 and return 42 as its result. I'll include a bit of FFI to instantiate and run the generated Wasm.

```purescript
module Main where

import Prelude

import Data.ArrayBuffer.Types (Uint8Array)
import Effect (Effect)
import Wasm.Encode as Encode
import Wasm.Syntax as S

foreign import execWasm :: Uint8Array -> Effect Unit

wasmModule :: S.Module
wasmModule = S.emptyModule
  { types = [ { arguments: [], results: [ S.NumType S.I32 ] } ]
  , funcs = [ { locals: [], type: 0, body: [ S.I32Const 20, S.I32Const 22, S.I32Add ] } ]
  , exports = [ { name: "main", desc: S.ExportFunc 0 } ]
  }

main :: Effect Unit
main = do
  let bytes = Encode.encodeModule wasmModule
  execWasm bytes
```

```javascript
export const execWasm = (bytes) => () => {
  WebAssembly.instantiate(bytes, {})
    .then(module => console.log(module.instance.exports.main()))
}
```

For a real compiler project you'll likely want to define your own builder on top of `Wasm.Syntax` to keep track of indices for you. An example of this can be seen in `example/src/WasmBuilder.purs`.