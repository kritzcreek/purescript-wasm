module Wasm.Encode where

import Prelude

import Data.Array as Array
import Data.Foldable (sequence_)
import Data.Int.Bits as Bits
import Data.Maybe (Maybe(..))
import DynamicBuffer (DBuffer)
import DynamicBuffer as DBuffer
import Effect (Effect)
import Partial.Unsafe (unsafeCrashWith)
import Wasm.Syntax as S

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

reserve_size :: DBuffer -> Effect Int
reserve_size b = do
  start <- DBuffer.get_position b
  DBuffer.add_int8 b 0
  DBuffer.add_int8 b 0
  DBuffer.add_int8 b 0
  DBuffer.add_int8 b 0
  DBuffer.add_int8 b 0
  pure start

write_reserved_size :: DBuffer -> Int -> Int -> Effect Unit
write_reserved_size b offset s = do
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

write_value_type :: DBuffer -> S.ValType -> Effect Unit
write_value_type b ty = DBuffer.add_int8 b case ty of
  S.I32 -> 0x7F
  S.I64 -> 0x7E
  S.F32 -> 0x7D
  S.F64 -> 0x7C

write_result_type :: DBuffer -> S.ResultType -> Effect Unit
write_result_type b ty = write_vec b (map (write_value_type b) ty)

write_func_type :: DBuffer -> S.FuncType -> Effect Unit
write_func_type b { arguments, results } = do
  DBuffer.add_int8 b 0x60
  write_result_type b arguments
  write_result_type b results

write_type_section :: DBuffer -> Array S.FuncType -> Effect Unit
write_type_section b types = write_section b 1 do
  write_vec b (map (write_func_type b) types)

write_function_section :: DBuffer -> Array S.Func -> Effect Unit
write_function_section b funcs = write_section b 3 do
  write_vec b (map (write_u32 b <<< _.type) funcs)

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
  size_offset <- reserve_size b
  start <- DBuffer.get_position b
  write_func b
  end <- DBuffer.get_position b
  write_reserved_size b size_offset (end - start)

write_code_section :: DBuffer -> Effect Unit
write_code_section b = write_section b 10 do
  write_vec b [ write_code b ]

write_section :: DBuffer -> SectionId -> Effect Unit -> Effect Unit
write_section b id f = do
  DBuffer.add_int8 b id
  size_offset <- reserve_size b
  start <- DBuffer.get_position b
  f
  end <- DBuffer.get_position b
  let size = end - start
  write_reserved_size b size_offset size
  pure unit

write_elem_type :: DBuffer -> S.ElemType -> Effect Unit
write_elem_type b = case _ of
  S.FuncRef -> DBuffer.add_int8 b 0x70

write_limits :: DBuffer -> S.Limits -> Effect Unit
write_limits b { min, max } = case max of
  Nothing -> do
    DBuffer.add_int8 b 0x00
    unsigned_leb128 b min
  Just max' -> do
    DBuffer.add_int8 b 0x01
    unsigned_leb128 b min
    unsigned_leb128 b max'

write_table_type :: DBuffer -> S.TableType -> Effect Unit
write_table_type b { limits, elemtype } = do
  write_elem_type b elemtype
  write_limits b limits

write_mutability :: DBuffer -> S.Mutability -> Effect Unit
write_mutability b mut = DBuffer.add_int8 b case mut of
  S.Const -> 0x00
  S.Var -> 0x01

write_global_type :: DBuffer -> S.GlobalType -> Effect Unit
write_global_type b { mutability, type: ty } = do
  write_value_type b ty
  write_mutability b mutability

write_import_desc :: DBuffer -> S.ImportDesc -> Effect Unit
write_import_desc b = case _ of
  S.ImportFunc ix -> do
    DBuffer.add_int8 b 0x00
    unsigned_leb128 b ix
  S.ImportTable table_type -> do
    DBuffer.add_int8 b 0x01
    write_table_type b table_type
  S.ImportMemory limits -> do
    DBuffer.add_int8 b 0x02
    write_limits b limits
  S.ImportGlobal global_type -> do
    DBuffer.add_int8 b 0x03
    write_global_type b global_type

write_name :: DBuffer -> S.Name -> Effect Unit
write_name = DBuffer.add_utf8

write_import :: DBuffer -> S.Import -> Effect Unit
write_import b imp = do
  write_name b imp.module
  write_name b imp.name
  write_import_desc b imp.desc

write_import_section :: DBuffer -> Array S.Import -> Effect Unit
write_import_section b imports = write_section b 2
  (write_vec b (map (write_import b) imports))

write_table :: DBuffer -> S.Table -> Effect Unit
write_table b t = write_table_type b t.type

write_table_section :: DBuffer -> Array S.Table -> Effect Unit
write_table_section b tables = write_section b 4 do
  write_vec b (map (write_table b) tables)

write_module :: S.Module -> Effect DBuffer
write_module module_ = do
  b <- DBuffer.create 1024
  write_header b
  write_type_section b module_.types
  write_import_section b module_.imports
  write_function_section b module_.funcs
  write_table_section b module_.tables
  -- TODO: Continue below
  write_export_section b
  write_code_section b
  pure b
