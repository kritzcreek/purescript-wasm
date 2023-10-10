module Main where

import Prelude

import Compiler as Compiler
import CompilerNew as CompilerNew
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
import ParserNew as ParserNew
import PrinterNew as PrinterNew
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
add x y = {
  x + y
}

fib x = {
  if x < 3 {
    1
  } else {
    add (fib (x - 1)) (fib (x - 2))
  }
}

main _ = {
  let x = 10;
  x + x
}
"""

main :: Effect Unit
main = do
  case ParserNew.parseFuncs input of
    Left err -> Console.logShow err
    Right funcs -> do
      Console.log (PrinterNew.printFuncs funcs)
      let bytes = Encode.encodeModule (CompilerNew.compileFuncs funcs)
      writeToFile "bytes.wasm" bytes case _ of
        Nothing ->
          runWasm "bytes.wasm"
        Just err ->
          Console.log ("Failed to write the wasm module" <> show err)

-- main :: Effect Unit
-- main = do
--   case Parser.parseFuncs input of
--     Left err -> Console.logShow err
--     Right funcs -> do
--       let bytes = Encode.encodeModule (Compiler.compileFuncs funcs)
--       writeToFile "bytes.wasm" bytes case _ of
--         Nothing ->
--           runWasm "bytes.wasm"
--         Just err ->
--           Console.log ("Failed to write the wasm module" <> show err)

