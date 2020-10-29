module Main where

import Prelude

import Buffer as Buffer
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Exception (Error)

-- When writing vectors refer to
-- https://webassembly.github.io/spec/core/binary/conventions.html#vectors

foreign import runWasm :: String -> Effect Unit

makeWasmModule :: (Maybe Error -> Effect Unit) -> Effect Unit
makeWasmModule cb = do
  buf <- Buffer.alloc 8
  Buffer.writeUInt8 buf 0 0x00
  Buffer.writeUInt8 buf 1 0x61
  Buffer.writeUInt8 buf 2 0x73
  Buffer.writeUInt8 buf 3 0x6D
  Buffer.writeUInt8 buf 4 0x01
  Buffer.writeToFile "byte.wasm" buf cb

main :: Effect Unit
main = do
  makeWasmModule case _ of
    Nothing ->
      runWasm "byte.wasm"
    Just err ->
      Console.log "Failed to write the wasm module"
