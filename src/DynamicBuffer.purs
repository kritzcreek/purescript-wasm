module DynamicBuffer
  ( DBuffer
  , create
  , add_int8
  , add_buffer

  , from_utf8

  , get_bytes
  , get_position

  , set
  , writeToFile
  , print
  ) where

import Prelude

import Data.ArrayBuffer.Types (Uint8Array)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect (Effect, whileE)
import Effect.Exception (Error, throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, mkEffectFn1, runEffectFn1, runEffectFn2, runEffectFn3)

type Offset = Int

foreign import lengthImpl :: EffectFn1 Uint8Array Int
foreign import setImpl :: EffectFn3 Uint8Array Offset Int Unit
foreign import setAllImpl :: EffectFn3 Uint8Array Offset Uint8Array Unit
foreign import allocate :: EffectFn1 Int Uint8Array
foreign import subarray :: EffectFn3 Uint8Array Offset Int Uint8Array
foreign import whenE :: EffectFn2 Boolean (Effect Unit) Unit

newtype DBuffer = DBuffer
  { bytes :: Ref Uint8Array
  , position :: Ref Int
  , length :: Ref Int
  }

create :: Int -> Effect DBuffer
create n = do
  bytes' <- runEffectFn1 allocate n
  bytes <- Ref.new bytes'
  position <- Ref.new 0
  length <- Ref.new n
  pure (DBuffer { bytes, position, length })

resize :: DBuffer -> Int -> Effect Unit
resize (DBuffer buf) more = do
  old_len <- Ref.read buf.length
  new_len <- Ref.new old_len
  -- TODO: Check we're not overshooting 32bit
  whileE (map (_ < old_len + more) (Ref.read new_len))
    (Ref.modify (_ * 2) new_len)
  new_len' <- Ref.read new_len
  new_bytes <- runEffectFn1 allocate new_len'
  old_bytes <- Ref.read buf.bytes
  runEffectFn3 setAllImpl new_bytes 0 old_bytes
  Ref.write new_bytes buf.bytes
  Ref.write new_len' buf.length

add_int8 :: DBuffer -> Int -> Effect Unit
add_int8 b@(DBuffer buf) x = do
  runEffectFn2 whenE (x < 0 || x >= 256)
    (throw ("add_int8: Out of range" <> show x))

  length <- Ref.read buf.length
  position <- Ref.read buf.position
  runEffectFn2 whenE (position >= length) (resize b 1)

  bytes <- Ref.read buf.bytes
  runEffectFn3 setImpl bytes position x
  Ref.write (position + 1) buf.position

add_buffer :: DBuffer -> DBuffer -> Effect Unit
add_buffer b@(DBuffer buf) x@(DBuffer xb) = do
  xb_position <- Ref.read xb.position
  buf_position <- Ref.read buf.position
  buf_len <- Ref.read buf.length
  let new_position = buf_position + xb_position
  runEffectFn2 whenE (new_position > buf_len) (resize b xb_position)

  xb_bytes <- Ref.read xb.bytes
  xb_sub <- runEffectFn3 subarray xb_bytes 0 xb_position
  bytes <- Ref.read buf.bytes
  runEffectFn3 setAllImpl bytes buf_position xb_sub

  Ref.write new_position buf.position

foreign import encode_utf8 :: EffectFn1 String Uint8Array

from_utf8 :: String -> Effect DBuffer
from_utf8 s = do
  bytes' <- runEffectFn1 encode_utf8 s
  length' <- runEffectFn1 lengthImpl bytes'
  bytes <- Ref.new bytes'
  length <- Ref.new length'
  position <- Ref.new length'
  pure (DBuffer { bytes, length, position })

get_bytes :: DBuffer -> Effect Uint8Array
get_bytes (DBuffer { bytes }) = Ref.read bytes

get_position :: DBuffer -> Effect Int
get_position (DBuffer { position }) = Ref.read position

set :: DBuffer -> Offset -> Int -> Effect Unit
set (DBuffer buf) offset x = do
  bytes <- Ref.read buf.bytes
  runEffectFn3 setImpl bytes offset x

foreign import writeToFileImpl ::
  EffectFn3
  String
  Uint8Array
  (EffectFn1 (Nullable Error) Unit)
  Unit

-- foreign import subarray :: EffectFn3 Uint8Array Offset Int Uint8Array

writeToFile :: String -> DBuffer -> (Maybe Error -> Effect Unit) -> Effect Unit
writeToFile path (DBuffer buf) cb = do
  bytes <- Ref.read buf.bytes
  position <- Ref.read buf.position
  used_bytes <- runEffectFn3 subarray bytes 0 position
  runEffectFn3 writeToFileImpl path used_bytes (mkEffectFn1 \err -> cb (Nullable.toMaybe err))

foreign import printImpl :: Uint8Array -> Effect Unit

print :: DBuffer -> Effect Unit
print (DBuffer buf) = do
  bytes <- Ref.read buf.bytes
  position <- Ref.read buf.position
  used_bytes <- runEffectFn3 subarray bytes 0 position
  printImpl used_bytes
