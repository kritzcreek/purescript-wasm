module Main where

import Prelude

import Compiler as Compiler
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
import Parser as Parser
import Wasm.Encode as Encode

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

input :: String
input = """
fn add(x, y) {
  x + y
}

fn fib(x) {
  let two = 2;
  if x < two {
    1
  } else {
    add(fib(x - 1), fib(x - 2))
  }
}

fn main() {
  fib(10)
}
"""

main :: Effect Unit
main = do
  Console.logShow (Parser.parseFuncs input)
  -- Console.logShow Compiler.testModule
  -- buf <- Encode.write_module Compiler.testModule
  -- writeToFile "bytes.wasm" buf case _ of
  --   Nothing ->
  --     runWasm "bytes.wasm"
  --   Just err ->
  --     Console.log "Failed to write the wasm module"
