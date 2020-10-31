module Wasm.Encode where

import Prelude

import Effect.Console as Console
import Data.Array as Array
import Data.Foldable (sequence_)
import Data.Int.Bits as Bits
import Data.Maybe (Maybe)
import DynamicBuffer (DBuffer)
import DynamicBuffer as DBuffer
import Effect (Effect)
import Effect.Exception (Error)

write_header :: DBuffer -> Effect Unit
write_header b = do
  DBuffer.add_int8 b 0x00
  DBuffer.add_int8 b 0x61
  DBuffer.add_int8 b 0x73
  DBuffer.add_int8 b 0x6D
  DBuffer.add_int8 b 0x01
  DBuffer.add_int8 b 0x00
  DBuffer.add_int8 b 0x00
  DBuffer.add_int8 b 0x00

{-
(module
  (func 0 (param 0 i32) (result i32)
    local.get 0
    i32.const 10
    i32.add)
  (export "addten" (func 0)))
-}

reserve_size_bytes :: DBuffer -> Effect Int
reserve_size_bytes b = do
  start <- DBuffer.get_position b
  DBuffer.add_int8 b 0
  DBuffer.add_int8 b 0
  DBuffer.add_int8 b 0
  DBuffer.add_int8 b 0
  DBuffer.add_int8 b 0
  pure start

write_section_size :: DBuffer -> Int -> Int -> Effect Unit
write_section_size b offset s = do
  let size = s + 0
  let chunk1 = (s `Bits.and` 0x7f) `Bits.or` 0x80
  let chunk2 = ((Bits.shr s 7) `Bits.and` 0x7f) `Bits.or` 0x80
  let chunk3 = ((Bits.shr s 14) `Bits.and` 0x7f) `Bits.or` 0x80
  let chunk4 = ((Bits.shr s 21) `Bits.and` 0x7f) `Bits.or` 0x80
  let chunk5 = (Bits.shr s 28) `Bits.and` 0x7f
  DBuffer.set b offset       chunk1
  DBuffer.set b (offset + 1) chunk2
  DBuffer.set b (offset + 2) chunk3
  DBuffer.set b (offset + 3) chunk4
  DBuffer.set b (offset + 4) chunk5

write_u32 :: DBuffer -> Int -> Effect Unit
write_u32 b s = do
  let size = s + 0
  let chunk1 = (s `Bits.and` 0x7f) `Bits.or` 0x80
  let chunk2 = ((Bits.shr s 7) `Bits.and` 0x7f) `Bits.or` 0x80
  let chunk3 = ((Bits.shr s 14) `Bits.and` 0x7f) `Bits.or` 0x80
  let chunk4 = ((Bits.shr s 21) `Bits.and` 0x7f) `Bits.or` 0x80
  let chunk5 = (Bits.shr s 28) `Bits.and` 0x7f
  DBuffer.add_int8 b chunk1
  DBuffer.add_int8 b chunk2
  DBuffer.add_int8 b chunk3
  DBuffer.add_int8 b chunk4
  DBuffer.add_int8 b chunk5

write_vec :: DBuffer -> Array (Effect Unit) -> Effect Unit
write_vec b elements = do
  let length = Array.length elements
  Console.log ("String length: " <> show length)
  -- DBuffer.add_int8 b length
  write_u32 b length
  sequence_ elements

write_custom_section :: DBuffer -> Effect Unit
write_custom_section b = do
  write_section b 0 do
    write_vec b [ DBuffer.add_int8 b 0x50, DBuffer.add_int8 b 0x53 ]

type SectionId = Int

write_section :: DBuffer -> SectionId -> Effect Unit -> Effect Unit
write_section b id f = do
  DBuffer.add_int8 b id
  size_offset <- reserve_size_bytes b
  start <- DBuffer.get_position b
  f
  end <- DBuffer.get_position b
  let size = end - start
  write_section_size b size_offset size
  pure unit

write_module :: (Maybe Error -> Effect Unit) -> Effect Unit
write_module cb = do
  b <- DBuffer.create 1024
  write_header b
  write_custom_section b
  DBuffer.writeToFile "bytes.wasm" b cb
