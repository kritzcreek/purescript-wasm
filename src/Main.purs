module Main where

import Prelude

import Data.Maybe (Maybe(..))
import DynamicBuffer as DBuffer
import Effect (Effect)
import Effect.Class.Console as Console
import Wasm.Encode as Encode
import Wasm.Syntax as Syntax

testModule :: Syntax.Module
testModule =
  Syntax.emptyModule {
    types = [ { arguments: [Syntax.I32, Syntax.I32], results: [Syntax.I32] } ],
    funcs = [ { type: 0, locals: [], body: [ Syntax.LocalGet 0, Syntax.LocalGet 1, Syntax.I32Add ] } ],
    exports = [ { name: "myadd", desc: Syntax.ExportFunc 0 }]
  }


foreign import runWasm :: String -> Effect Unit

main :: Effect Unit
main = do
  buf <- Encode.write_module testModule
  DBuffer.writeToFile "bytes.wasm" buf case _ of
    Nothing ->
      runWasm "bytes.wasm"
    Just err ->
      Console.log "Failed to write the wasm module"
