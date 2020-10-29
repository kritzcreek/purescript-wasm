module Buffer
  ( Buffer
  , Offset
  , alloc
  , writeUInt8
  , toString
  , writeToFile
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, mkEffectFn1)

type Offset = Int

foreign import data Buffer :: Type

foreign import alloc :: Int -> Effect Buffer
foreign import writeUInt8 :: Buffer -> Offset -> Int -> Effect Unit
foreign import toString :: Buffer -> String
foreign import writeToFileImpl ::
  String ->
  Buffer ->
  (EffectFn1 (Nullable Error) Unit) ->
  Effect Unit

writeToFile :: String -> Buffer -> (Maybe Error -> Effect Unit) -> Effect Unit
writeToFile path buf cb =
  writeToFileImpl path buf (mkEffectFn1 \err -> cb (Nullable.toMaybe err))
