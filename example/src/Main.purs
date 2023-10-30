module Main where

import Prelude

import Data.ArrayBuffer.Types (Uint8Array)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Driver as Driver
import Effect (Effect)
import Effect.Class.Console as Console
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, EffectFn3, mkEffectFn1, runEffectFn1, runEffectFn3)

foreign import writeToFileImpl
  :: EffectFn3
       String
       Uint8Array
       (EffectFn1 (Nullable Error) Unit)
       Unit

writeToFile :: String -> Uint8Array -> (Maybe Error -> Effect Unit) -> Effect Unit
writeToFile path bytes cb =
  runEffectFn3 writeToFileImpl path bytes (mkEffectFn1 \err -> cb (Nullable.toMaybe err))

foreign import runWasmImpl :: EffectFn1 String Unit

runWasm :: String -> Effect Unit
runWasm = runEffectFn1 runWasmImpl

input :: String
input =
  """
struct V2 { x : f32, y : f32 }

fn main() : f32 = {
  let v = V2 { x = 1.0, y = 2.0 };
  v.x
}
"""

main :: Effect Unit
main = do
  let bytes = Driver.compileProgram input
  writeToFile "bytes.wasm" bytes case _ of
    Nothing ->
      pure unit
    -- runWasm "bytes.wasm"
    Just err ->
      Console.log ("Failed to write the wasm module" <> show err)
