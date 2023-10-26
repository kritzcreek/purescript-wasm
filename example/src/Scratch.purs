module Scratch where

import Data.ArrayBuffer.Types (Uint8Array)
import Wasm.Encode as Encode
import Wasm.Syntax as S

wasmModule :: S.Module
wasmModule = S.emptyModule
  { types = [ [ { final: true, supertypes: [], ty: S.CompFunc { arguments: [], results: [ S.NumType S.I32 ] } } ] ]
  , funcs = [ { locals: [], type: 0, body: [ S.I32Const 20, S.I32Const 22, S.I32Add ] } ]
  , exports = [ { name: "main", desc: S.ExportFunc 0 } ]
  }

exampleModule :: Uint8Array
exampleModule = Encode.encodeModule wasmModule
