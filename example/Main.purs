module Main where

import Prelude

import Compiler as Compiler
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
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

writeToFile :: String -> Uint8Array -> (Maybe Error -> Effect Unit) -> Effect Unit
writeToFile path bytes cb =
  runEffectFn3 writeToFileImpl path bytes (mkEffectFn1 \err -> cb (Nullable.toMaybe err))

foreign import runWasm :: String -> Effect Unit

input :: String
input = """
fn add(x, y) {
  x + y
}

fn fib(x) {
  if x < 3 {
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
  case Parser.parseFuncs input of
    Left err -> Console.logShow err
    Right funcs -> do
      let bytes = Encode.encodeModule (Compiler.compileFuncs funcs)
      writeToFile "bytes.wasm" bytes case _ of
          Nothing ->
            runWasm "bytes.wasm"
          Just err ->
            Console.log ("Failed to write the wasm module" <> show err)
