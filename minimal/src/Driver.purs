module Driver where

import Prelude

import Compiler as Compiler
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe as Maybe
import Parser as Parser
import Partial.Unsafe (unsafeCrashWith)
import Printer as Printer
import Rename as Rename
import Wasm.Encode as Encode

compileProgram :: String -> Uint8Array
compileProgram input = do
  case Parser.parseProgram input of
    Left err -> unsafeCrashWith ("Failed to parse with: " <> show err)
    Right program -> do
      let renamed = Rename.renameProgram program
      Encode.encodeModule (Compiler.compileProgram renamed.result)

renameProgram :: String -> String
renameProgram input =
  case Parser.parseProgram input of
    Left err -> unsafeCrashWith ("Failed to parse with: " <> show err)
    Right program -> do
      let { result, nameMap } = Rename.renameProgram program
      Printer.printProgram (\v -> Maybe.maybe "$UNKNOWN" (\n -> show v <> n) (Map.lookup v nameMap)) result
