module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console as Console
import Wasm.Encode as Encode

foreign import runWasm :: String -> Effect Unit

main :: Effect Unit
main = do
  Encode.write_module case _ of
    Nothing ->
      runWasm "bytes.wasm"
    Just err ->
      Console.log "Failed to write the wasm module"
