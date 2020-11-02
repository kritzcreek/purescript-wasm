module Main where

import Prelude

import Data.Maybe (Maybe(..))
import DynamicBuffer as DBuffer
import Effect (Effect)
import Effect.Class.Console as Console
import Wasm.Encode as Encode
import Wasm.Syntax as Syntax

foreign import runWasm :: String -> Effect Unit

main :: Effect Unit
main = do
  buf <- Encode.write_module Syntax.emptyModule
  DBuffer.writeToFile "bytes.wasm" buf case _ of
    Nothing ->
      runWasm "bytes.wasm"
    Just err ->
      Console.log "Failed to write the wasm module"
