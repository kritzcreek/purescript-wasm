module Main where

import Prelude

import Data.ArrayBuffer.Types (Uint8Array)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import DynamicBuffer (DBuffer)
import DynamicBuffer as DBuffer
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, EffectFn3, mkEffectFn1, runEffectFn3)
import Wasm.Encode as Encode
import Wasm.Syntax as Syntax

testModule :: Syntax.Module
testModule =
  Syntax.emptyModule {
    types = [ { arguments: [Syntax.I32, Syntax.I32], results: [Syntax.I32] } ],
    funcs = [ { type: 0, locals: [], body: [ Syntax.LocalGet 0, Syntax.LocalGet 1, Syntax.I32Add ] } ],
    exports = [ { name: "myadd", desc: Syntax.ExportFunc 0 }]
  }

foreign import writeToFileImpl ::
  EffectFn3
  String
  Uint8Array
  (EffectFn1 (Nullable Error) Unit)
  Unit

writeToFile :: String -> DBuffer -> (Maybe Error -> Effect Unit) -> Effect Unit
writeToFile path buf cb = do
  bytes <- DBuffer.getBytes buf
  runEffectFn3 writeToFileImpl path bytes (mkEffectFn1 \err -> cb (Nullable.toMaybe err))

foreign import runWasm :: String -> Effect Unit

main :: Effect Unit
main = do
  buf <- Encode.write_module testModule
  writeToFile "bytes.wasm" buf case _ of
    Nothing ->
      runWasm "bytes.wasm"
    Just err ->
      Console.log "Failed to write the wasm module"
