module Wasm.Encode (encodeModule) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Foldable (sequence_)
import Data.Int.Bits as Bits
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import DynamicBuffer (DBuffer)
import DynamicBuffer as DBuffer
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafeCrashWith)
import Wasm.Syntax (DataMode(..))
import Wasm.Syntax as S

unsigned_leb128 :: DBuffer -> Int -> Effect Unit
unsigned_leb128 b x =
  if x < 0 then unsafeCrashWith "Negative unsigned_leb128"
  else
    let
      seven_bits = x `Bits.and` 0x7F
    in
      let
        shifted = x `Bits.shr` 7
      in
        if shifted == 0 then DBuffer.addByte b seven_bits
        else do
          DBuffer.addByte b (seven_bits `Bits.or` 0x80)
          unsigned_leb128 b shifted

signed_leb128 :: DBuffer -> Int -> Effect Unit
signed_leb128 b x =
  let
    seven_bits = x `Bits.and` 0x7F
  in
    let
      shifted = x `Bits.shr` 7
    in
      if
        (shifted == 0 && seven_bits `Bits.and` 0x40 == 0)
          || (shifted == -1 && seven_bits `Bits.and` 0x40 /= 0) then DBuffer.addByte b seven_bits
      else do
        DBuffer.addByte b (seven_bits `Bits.or` 0x80)
        signed_leb128 b shifted

write_header :: DBuffer -> Effect Unit
write_header b = do
  DBuffer.addByte b 0x00
  DBuffer.addByte b 0x61
  DBuffer.addByte b 0x73
  DBuffer.addByte b 0x6D
  DBuffer.addByte b 0x01
  DBuffer.addByte b 0x00
  DBuffer.addByte b 0x00
  DBuffer.addByte b 0x00

reserve_size :: DBuffer -> Effect Int
reserve_size b = do
  start <- DBuffer.size b
  DBuffer.addByte b 0
  DBuffer.addByte b 0
  DBuffer.addByte b 0
  DBuffer.addByte b 0
  DBuffer.addByte b 0
  pure start

write_reserved_size :: DBuffer -> Int -> Int -> Effect Unit
write_reserved_size b offset s = do
  let chunk1 = (s `Bits.and` 0x7f) `Bits.or` 0x80
  let chunk2 = ((Bits.shr s 7) `Bits.and` 0x7f) `Bits.or` 0x80
  let chunk3 = ((Bits.shr s 14) `Bits.and` 0x7f) `Bits.or` 0x80
  let chunk4 = ((Bits.shr s 21) `Bits.and` 0x7f) `Bits.or` 0x80
  let chunk5 = (Bits.shr s 28) `Bits.and` 0x7f
  DBuffer.setByte b offset chunk1
  DBuffer.setByte b (offset + 1) chunk2
  DBuffer.setByte b (offset + 2) chunk3
  DBuffer.setByte b (offset + 3) chunk4
  DBuffer.setByte b (offset + 4) chunk5

withSize :: forall a. DBuffer -> Effect a -> Effect a
withSize b f = do
  size_offset <- reserve_size b
  start <- DBuffer.size b
  a <- f
  end <- DBuffer.size b
  let size = end - start
  write_reserved_size b size_offset size
  pure a

write_u32 :: DBuffer -> Int -> Effect Unit
write_u32 = unsigned_leb128

write_vec :: DBuffer -> Array (Effect Unit) -> Effect Unit
write_vec b elements = do
  write_u32 b (Array.length elements)
  sequence_ elements

type SectionId = Int

write_num_type :: DBuffer -> S.NumType -> Effect Unit
write_num_type b ty = DBuffer.addByte b case ty of
  S.I32 -> 0x7F
  S.I64 -> 0x7E
  S.F32 -> 0x7D
  S.F64 -> 0x7C

write_value_type :: DBuffer -> S.ValType -> Effect Unit
write_value_type b = case _ of
  S.NumType ty -> write_num_type b ty
  S.RefType ty -> write_ref_type b ty

write_result_type :: DBuffer -> S.ResultType -> Effect Unit
write_result_type b ty = write_vec b (map (write_value_type b) ty)

write_func_type :: DBuffer -> S.FuncType -> Effect Unit
write_func_type b { arguments, results } = do
  DBuffer.addByte b 0x60
  write_result_type b arguments
  write_result_type b results

write_type_section :: DBuffer -> Array S.FuncType -> Effect Unit
write_type_section b types = write_section b 1 do
  write_vec b (map (write_func_type b) types)

write_function_section :: DBuffer -> Array S.Func -> Effect Unit
write_function_section b funcs = write_section b 3 do
  write_vec b (map (write_u32 b <<< _.type) funcs)

write_memarg :: DBuffer -> S.MemArg -> Effect Unit
write_memarg b { align, offset } = do
  write_u32 b align
  write_u32 b offset

write_block_type :: DBuffer -> S.BlockType -> Effect Unit
write_block_type b = case _ of
  S.BlockValType Nothing ->
    DBuffer.addByte b 0x40
  S.BlockValType (Just ty) ->
    write_value_type b ty
  S.BlockTypeIdx idx ->
    signed_leb128 b idx

write_instr :: DBuffer -> S.Instruction -> Effect Unit
write_instr b = case _ of
  S.I32Const n -> do
    DBuffer.addByte b 0x41
    signed_leb128 b n
  S.I32Clz ->
    DBuffer.addByte b 0x67
  S.I32Ctz ->
    DBuffer.addByte b 0x68
  S.I32Popcnt ->
    DBuffer.addByte b 0x69
  S.I32Add ->
    DBuffer.addByte b 0x6A
  S.I32Sub ->
    DBuffer.addByte b 0x6B
  S.I32Mul ->
    DBuffer.addByte b 0x6C
  S.I32Div_s ->
    DBuffer.addByte b 0x6D
  S.I32Div_u ->
    DBuffer.addByte b 0x6E
  S.I32Rem_s ->
    DBuffer.addByte b 0x6F
  S.I32Rem_u ->
    DBuffer.addByte b 0x70
  S.I32And ->
    DBuffer.addByte b 0x71
  S.I32Or ->
    DBuffer.addByte b 0x72
  S.I32Xor ->
    DBuffer.addByte b 0x73
  S.I32Shl ->
    DBuffer.addByte b 0x74
  S.I32Shr_s ->
    DBuffer.addByte b 0x75
  S.I32Shr_u ->
    DBuffer.addByte b 0x76
  S.I32Rotl ->
    DBuffer.addByte b 0x77
  S.I32Rotr ->
    DBuffer.addByte b 0x78
  S.I32Eqz ->
    DBuffer.addByte b 0x45
  S.I32Eq ->
    DBuffer.addByte b 0x46
  S.I32Ne ->
    DBuffer.addByte b 0x47
  S.I32Lt_s ->
    DBuffer.addByte b 0x48
  S.I32Lt_u ->
    DBuffer.addByte b 0x49
  S.I32Gt_s ->
    DBuffer.addByte b 0x4A
  S.I32Gt_u ->
    DBuffer.addByte b 0x4B
  S.I32Le_s ->
    DBuffer.addByte b 0x4C
  S.I32Le_u ->
    DBuffer.addByte b 0x4D
  S.I32Ge_s ->
    DBuffer.addByte b 0x4E
  S.I32Ge_u ->
    DBuffer.addByte b 0x4F
  S.I32Extend8_s ->
    DBuffer.addByte b 0xC0
  S.I32Extend16_s ->
    DBuffer.addByte b 0xC1
  S.I32Wrap_i64 ->
    DBuffer.addByte b 0xA7
  S.Drop ->
    DBuffer.addByte b 0x1A
  S.Select ->
    DBuffer.addByte b 0x1B
  S.LocalGet idx -> do
    DBuffer.addByte b 0x20
    write_u32 b idx
  S.LocalSet idx -> do
    DBuffer.addByte b 0x21
    write_u32 b idx
  S.LocalTee idx -> do
    DBuffer.addByte b 0x22
    write_u32 b idx
  S.GlobalGet idx -> do
    DBuffer.addByte b 0x23
    write_u32 b idx
  S.GlobalSet idx -> do
    DBuffer.addByte b 0x24
    write_u32 b idx
  S.I32Load memarg -> do
    DBuffer.addByte b 0x28
    write_memarg b memarg
  S.I32Load8_s memarg -> do
    DBuffer.addByte b 0x2C
    write_memarg b memarg
  S.I32Load8_u memarg -> do
    DBuffer.addByte b 0x2D
    write_memarg b memarg
  S.I32Load16_s memarg -> do
    DBuffer.addByte b 0x2E
    write_memarg b memarg
  S.I32Load16_u memarg -> do
    DBuffer.addByte b 0x2F
    write_memarg b memarg
  S.I32Store memarg -> do
    DBuffer.addByte b 0x36
    write_memarg b memarg
  S.I32Store8 memarg -> do
    DBuffer.addByte b 0x3A
    write_memarg b memarg
  S.I32Store16 memarg -> do
    DBuffer.addByte b 0x3B
    write_memarg b memarg
  S.MemorySize -> do
    DBuffer.addByte b 0x3F
    DBuffer.addByte b 0x00
  S.MemoryGrow -> do
    DBuffer.addByte b 0x40
    DBuffer.addByte b 0x00
  S.MemoryInit x -> do
    DBuffer.addByte b 0xFC
    unsigned_leb128 b 8
    unsigned_leb128 b x
    DBuffer.addByte b 0x00
  S.DataDrop x -> do
    DBuffer.addByte b 0xFC
    unsigned_leb128 b 9
    unsigned_leb128 b x
  S.MemoryCopy -> do
    DBuffer.addByte b 0xFC
    unsigned_leb128 b 10
    unsigned_leb128 b 0x00
    unsigned_leb128 b 0x00
  S.MemoryFill -> do
    DBuffer.addByte b 0xFC
    unsigned_leb128 b 11
    unsigned_leb128 b 0x00
  S.Unreachable ->
    DBuffer.addByte b 0x00
  S.Nop ->
    DBuffer.addByte b 0x01
  S.Block ty instrs -> do
    DBuffer.addByte b 0x02
    write_block_type b ty
    for_ instrs (write_instr b)
    DBuffer.addByte b 0x0B
  S.Loop ty instrs -> do
    DBuffer.addByte b 0x03
    write_block_type b ty
    for_ instrs (write_instr b)
    DBuffer.addByte b 0x0B
  S.If ty thn els -> do
    DBuffer.addByte b 0x04
    write_block_type b ty
    for_ thn (write_instr b)
    unless (Array.null els) do
      DBuffer.addByte b 0x05
      for_ els (write_instr b)
    DBuffer.addByte b 0x0B
  S.Br labelidx -> do
    DBuffer.addByte b 0x0C
    write_u32 b labelidx
  S.Br_if labelidx -> do
    DBuffer.addByte b 0x0D
    write_u32 b labelidx
  S.Br_table labelindxs labelidx -> do
    DBuffer.addByte b 0x0E
    write_vec b (map (write_u32 b) labelindxs)
    write_u32 b labelidx
  S.Return ->
    DBuffer.addByte b 0x0F
  S.Call idx -> do
    DBuffer.addByte b 0x10
    write_u32 b idx
  S.Call_Indirect type_idx -> do
    DBuffer.addByte b 0x11
    write_u32 b type_idx
    DBuffer.addByte b 0x00

write_expr :: DBuffer -> S.Expr -> Effect Unit
write_expr b instrs = do
  for_ instrs (write_instr b)
  DBuffer.addByte b 0x0B

write_section :: DBuffer -> SectionId -> Effect Unit -> Effect Unit
write_section b id f = do
  DBuffer.addByte b id
  withSize b f

write_limits :: DBuffer -> S.Limits -> Effect Unit
write_limits b { min, max } = case max of
  Nothing -> do
    DBuffer.addByte b 0x00
    write_u32 b min
  Just max' -> do
    DBuffer.addByte b 0x01
    write_u32 b min
    write_u32 b max'

write_ref_type :: DBuffer -> S.RefType -> Effect Unit
write_ref_type b = case _ of
  S.FuncRef -> DBuffer.addByte b 0x70
  S.ExternRef -> DBuffer.addByte b 0x6F

write_table_type :: DBuffer -> S.TableType -> Effect Unit
write_table_type b { limits, elemtype } = do
  write_ref_type b elemtype
  write_limits b limits

write_mutability :: DBuffer -> S.Mutability -> Effect Unit
write_mutability b mut = DBuffer.addByte b case mut of
  S.Const -> 0x00
  S.Var -> 0x01

write_global_type :: DBuffer -> S.GlobalType -> Effect Unit
write_global_type b { mutability, type: ty } = do
  write_value_type b ty
  write_mutability b mutability

write_import_desc :: DBuffer -> S.ImportDesc -> Effect Unit
write_import_desc b = case _ of
  S.ImportFunc ix -> do
    DBuffer.addByte b 0x00
    write_u32 b ix
  S.ImportTable table_type -> do
    DBuffer.addByte b 0x01
    write_table_type b table_type
  S.ImportMemory limits -> do
    DBuffer.addByte b 0x02
    write_limits b limits
  S.ImportGlobal global_type -> do
    DBuffer.addByte b 0x03
    write_global_type b global_type

write_name :: DBuffer -> S.Name -> Effect Unit
write_name b name = do
  buf <- DBuffer.fromUtf8 name
  len <- DBuffer.size buf
  write_u32 b len
  DBuffer.addBuffer b buf

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

write_mem_type :: DBuffer -> S.MemoryType -> Effect Unit
write_mem_type b mem_ty = write_limits b mem_ty

write_memory_section :: DBuffer -> Array S.Memory -> Effect Unit
write_memory_section b memories = write_section b 5 do
  write_vec b (map (write_mem_type b <<< _.type) memories)

write_global :: DBuffer -> S.Global -> Effect Unit
write_global b global = do
  write_global_type b global.type
  write_expr b global.init

write_global_section :: DBuffer -> Array S.Global -> Effect Unit
write_global_section b globals = write_section b 6 do
  write_vec b (map (write_global b) globals)

write_export_desc :: DBuffer -> S.ExportDesc -> Effect Unit
write_export_desc b = case _ of
  S.ExportFunc idx -> do
    DBuffer.addByte b 0x00
    write_u32 b idx
  S.ExportTable idx -> do
    DBuffer.addByte b 0x01
    write_u32 b idx
  S.ExportMemory idx -> do
    DBuffer.addByte b 0x02
    write_u32 b idx
  S.ExportGlobal idx -> do
    DBuffer.addByte b 0x03
    write_u32 b idx

write_export :: DBuffer -> S.Export -> Effect Unit
write_export b { name, desc } = do
  write_name b name
  write_export_desc b desc

write_export_section :: DBuffer -> Array S.Export -> Effect Unit
write_export_section b exports = write_section b 7 do
  write_vec b (map (write_export b) exports)

write_start_section :: DBuffer -> S.FuncIdx -> Effect Unit
write_start_section b idx = write_section b 8 (write_u32 b idx)

write_elem :: DBuffer -> S.Elem -> Effect Unit
write_elem b { table, offset, init } = do
  write_u32 b table
  write_expr b offset
  write_vec b (map (write_u32 b) init)

write_elem_section :: DBuffer -> Array S.Elem -> Effect Unit
write_elem_section b elems = write_section b 9 do
  write_vec b (map (write_elem b) elems)

write_locals :: DBuffer -> Array S.ValType -> Array (Effect Unit)
write_locals b locals = do
  let grouped = Array.groupAll locals
  map
    ( \tys -> do
        write_u32 b (NEA.length tys)
        write_value_type b (NEA.head tys)
    )
    grouped

write_code :: DBuffer -> S.Func -> Effect Unit
write_code b func = withSize b do
  write_vec b (write_locals b func.locals)
  write_expr b func.body

write_code_section :: DBuffer -> Array S.Func -> Effect Unit
write_code_section b funcs = write_section b 10 do
  write_vec b (map (write_code b) funcs)

write_data :: DBuffer -> S.Data -> Effect Unit
write_data b dat = case dat.mode of
  Passive -> do
    DBuffer.addByte b 0x01
    write_vec b (map (DBuffer.addByte b) dat.init)
  Active { memory, offset }
    | memory == 0 -> do
        DBuffer.addByte b 0x00
        write_expr b offset
        write_vec b (map (DBuffer.addByte b) dat.init)
    | otherwise -> do
        DBuffer.addByte b 0x02
        write_u32 b memory
        write_expr b offset
        write_vec b (map (DBuffer.addByte b) dat.init)

write_data_section :: DBuffer -> Array S.Data -> Effect Unit
write_data_section b datas = write_section b 11 do
  write_vec b (map (write_data b) datas)

write_module :: S.Module -> Effect DBuffer
write_module module_ = do
  b <- DBuffer.create 8192
  write_header b
  unless (Array.null module_.types) (write_type_section b module_.types)
  unless (Array.null module_.imports) (write_import_section b module_.imports)
  unless (Array.null module_.funcs) (write_function_section b module_.funcs)
  unless (Array.null module_.tables) (write_table_section b module_.tables)
  unless (Array.null module_.memories) (write_memory_section b module_.memories)
  unless (Array.null module_.globals) (write_global_section b module_.globals)
  unless (Array.null module_.exports) (write_export_section b module_.exports)
  for_ module_.start (write_start_section b)
  unless (Array.null module_.elem) (write_elem_section b module_.elem)
  unless (Array.null module_.funcs) (write_code_section b module_.funcs)
  unless (Array.null module_.data) (write_data_section b module_.data)
  pure b

-- | Encodes a Wasm module into its binary representation.
encodeModule :: S.Module -> Uint8Array
encodeModule module_ = unsafePerformEffect do
  buf <- write_module module_
  DBuffer.unsafeContents buf
