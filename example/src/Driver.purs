module Driver where

import Prelude

import Compiler as Compiler
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Either (Either(..))
import Parser as Parser
import Partial.Unsafe (unsafeCrashWith)
import Printer as Printer
import Rename as Rename
import Types as Types
import Wasm.Encode as Encode

compileProgram :: String -> Uint8Array
compileProgram input = do
  case Parser.parseProgram input of
    Left err -> unsafeCrashWith ("Failed to parse with: " <> show err)
    Right program -> do
      case Types.inferProgram program of
        Left err -> unsafeCrashWith ("Failed to typecheck with: " <> err)
        Right typed -> do
          let { result } = Rename.renameProgram typed
          Encode.encodeModule (Compiler.compileProgram result)

renameProgram :: String -> String
renameProgram input =
  case Parser.parseProgram input of
    Left err -> unsafeCrashWith ("Failed to parse with: " <> show err)
    Right program -> do
      case Types.inferProgram program of
        Left err -> unsafeCrashWith ("Failed to typecheck with: " <> err)
        Right typed -> do
          let { result, nameMap } = Rename.renameProgram typed
          Printer.printProgram (Printer.renderAll nameMap) result
