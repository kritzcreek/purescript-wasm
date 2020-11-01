module Wasm.Encode where

import Prelude

import Data.Array as Array
import Data.Foldable (sequence_)
import Data.Int.Bits as Bits
import Data.Maybe (Maybe)
import DynamicBuffer (DBuffer)
import DynamicBuffer as DBuffer
import Effect (Effect)
import Effect.Exception (Error)
import Partial.Unsafe (unsafeCrashWith)

unsigned_leb128 :: DBuffer -> Int -> Effect Unit
unsigned_leb128 b x =
  if x < 0
  then unsafeCrashWith "Negative unsigned_leb128"
  else
    let seven_bits = x `Bits.and` 0x7F in
    let shifted = x `Bits.shr` 7 in
    if shifted == 0
    then DBuffer.add_int8 b seven_bits
    else do
      DBuffer.add_int8 b (seven_bits `Bits.or` 0x80)
      unsigned_leb128 b shifted

signed_leb128 :: DBuffer -> Int -> Effect Unit
signed_leb128 b x =
  let seven_bits = x `Bits.and` 0x7F in
  let shifted = x `Bits.shr` 7 in
  if (shifted == 0 && seven_bits `Bits.and` 0x40 == 0)
     || (shifted == -1 && seven_bits `Bits.and` 0x40 /= 0)
  then DBuffer.add_int8 b seven_bits
  else do
    DBuffer.add_int8 b (seven_bits `Bits.or` 0x80)
    signed_leb128 b shifted

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

reserve_section_size :: DBuffer -> Effect Int
reserve_section_size b = do
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
write_u32 = unsigned_leb128

write_vec :: DBuffer -> Array (Effect Unit) -> Effect Unit
write_vec b elements = do
  write_u32 b (Array.length elements)
  sequence_ elements

write_custom_section :: DBuffer -> Effect Unit
write_custom_section b = do
  write_section b 0 do
    write_vec b [ DBuffer.add_int8 b 0x50, DBuffer.add_int8 b 0x53 ]

type SectionId = Int

write_result_type :: DBuffer -> Effect Unit
write_result_type b = write_vec b [ DBuffer.add_int8 b 0x7F ]

write_func_type :: DBuffer -> Effect Unit
write_func_type b = do
  DBuffer.add_int8 b 0x60
  write_result_type b
  write_result_type b

write_type_section :: DBuffer -> Effect Unit
write_type_section b = write_section b 1 do
  write_vec b [ write_func_type b ]

write_function_section :: DBuffer -> Effect Unit
write_function_section b = write_section b 3 do
  write_vec b [ DBuffer.add_int8 b 0 ]

write_export :: DBuffer -> Effect Unit
write_export b = do
  write_vec b
    [ DBuffer.add_int8 b 0x61
    , DBuffer.add_int8 b 0x64
    , DBuffer.add_int8 b 0x64
    ]
  DBuffer.add_int8 b 0x00
  DBuffer.add_int8 b 0x00

write_export_section :: DBuffer -> Effect Unit
write_export_section b = write_section b 7 do
  write_vec b [ write_export b ]

write_expr :: DBuffer -> Effect Unit
write_expr b = do
  -- local.get 0
  DBuffer.add_int8 b 0x20
  DBuffer.add_int8 b 0x00

  -- i32.const 10
  DBuffer.add_int8 b 0x41
  signed_leb128 b (-16452)

  -- i32.add
  DBuffer.add_int8 b 0x6A

  -- end
  DBuffer.add_int8 b 0x0B

write_func :: DBuffer -> Effect Unit
write_func b = do
  write_vec b [ DBuffer.add_int8 b 1 *> DBuffer.add_int8 b 0x7F ]
  write_expr b

write_code :: DBuffer -> Effect Unit
write_code b = do
  size_offset <- reserve_section_size b
  start <- DBuffer.get_position b
  write_func b
  end <- DBuffer.get_position b
  write_section_size b size_offset (end - start)

write_code_section :: DBuffer -> Effect Unit
write_code_section b = write_section b 10 do
  write_vec b [ write_code b ]

write_section :: DBuffer -> SectionId -> Effect Unit -> Effect Unit
write_section b id f = do
  DBuffer.add_int8 b id
  size_offset <- reserve_section_size b
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
  write_type_section b
  write_function_section b
  write_export_section b
  write_code_section b
  DBuffer.writeToFile "bytes.wasm" b cb
