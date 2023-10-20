module Driver where

import Prelude

import Compiler as Compiler
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Either (Either(..))
import Parser as Parser
import Partial.Unsafe (unsafeCrashWith)
import Wasm.Encode as Encode

compileProgram :: String -> Uint8Array
compileProgram input = do
  case Parser.parseToplevel input of
    Left err -> unsafeCrashWith ("Failed to parse with: " <> show err)
    Right toplevels ->
      Encode.encodeModule (Compiler.compileToplevels toplevels)
