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
import Effect.Uncurried (EffectFn1, EffectFn3, mkEffectFn1, runEffectFn1, runEffectFn3)
import Parser as Parser
import Printer as Printer
import Rename as Rename
import Wasm.Encode as Encode

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
import draw_line : (i32, i32, i32, i32) -> i32 from draw_line
import clear : () -> i32 from clear_canvas

let x = 0;
let y = 500;

fn max(x : i32, y : i32) : i32 =
  if x > y {
    x
  } else {
    y
  }

fn min(x : i32, y : i32) : i32 =
  if x < y {
    x
  } else {
    y
  }

fn clamp(lo : i32, x : i32, hi : i32) : i32 = min(hi, max(lo, x))

fn tick() = {
  clear();
  set x = clamp(0, x + 5, 500);
  set y = clamp(0, y - 10, 500);
  draw_line(0, 0, x, y)
}
"""

main :: Effect Unit
main = do
  case Parser.parseProgram input of
    Left err -> Console.logShow err
    Right program -> do
      Console.log (Printer.printProgram identity program)
      let
        renamed = Rename.renameProgram program
        bytes = Encode.encodeModule (Compiler.compileProgram renamed.result)
      writeToFile "playground/bytes.wasm" bytes case _ of
        Nothing ->
          pure unit
        -- runWasm "bytes.wasm"
        Just err ->
          Console.log ("Failed to write the wasm module" <> show err)
