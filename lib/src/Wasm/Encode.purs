module Wasm.Encode where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Foldable (sequence_)
import Data.Int.Bits as Bits
import Data.Maybe (Maybe(..))
import Data.Traversable (for_, traverse)
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
  else do
    let seven_bits = x `Bits.and` 0x7F
    let shifted = x `Bits.shr` 7
    if shifted == 0 then
      DBuffer.addByte b seven_bits
    else do
      DBuffer.addByte b (seven_bits `Bits.or` 0x80)
      unsigned_leb128 b shifted

signed_leb128 :: DBuffer -> Int -> Effect Unit
signed_leb128 b x = do
  let seven_bits = x `Bits.and` 0x7F
  let shifted = x `Bits.shr` 7
  if
    (shifted == 0 && seven_bits `Bits.and` 0x40 == 0)
      || (shifted == -1 && seven_bits `Bits.and` 0x40 /= 0) then DBuffer.addByte b seven_bits
  else do
    DBuffer.addByte b (seven_bits `Bits.or` 0x80)
    signed_leb128 b shifted

foreign import bytes_of_float32 :: Number -> Array Int

float32 :: DBuffer -> Number -> Effect Unit
float32 b x = for_ (bytes_of_float32 x) (DBuffer.addByte b)

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

-- Not sure if this is correct
write_s33 :: DBuffer -> Int -> Effect Unit
write_s33 = signed_leb128

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

write_packed_type :: DBuffer -> S.PackedType -> Effect Unit
write_packed_type b = case _ of
  S.I8 ->
    DBuffer.addByte b 0x78
  S.I16 ->
    DBuffer.addByte b 0x77

write_result_type :: DBuffer -> S.ResultType -> Effect Unit
write_result_type b ty = write_vec b (map (write_value_type b) ty)

write_func_type :: DBuffer -> S.FuncType -> Effect Unit
write_func_type b { arguments, results } = do
  write_result_type b arguments
  write_result_type b results

write_storage_type :: DBuffer -> S.StorageType -> Effect Unit
write_storage_type b = case _ of
  S.StorageVal t -> write_value_type b t
  S.StoragePacked t -> write_packed_type b t

write_field_type :: DBuffer -> S.FieldType -> Effect Unit
write_field_type b { mutability, ty } = do
  write_storage_type b ty
  write_mutability b mutability

write_comp_type :: DBuffer -> S.CompositeType -> Effect Unit
write_comp_type b = case _ of
  S.CompFunc t -> do
    DBuffer.addByte b 0x60
    write_func_type b t
  S.CompStruct ts -> do
    DBuffer.addByte b 0x5F
    write_vec b (map (write_field_type b) ts)
  S.CompArray t -> do
    DBuffer.addByte b 0x5E
    write_field_type b t

write_sub_type :: DBuffer -> S.SubType -> Effect Unit
write_sub_type b = case _ of
  { final: true, supertypes: [], ty } ->
    write_comp_type b ty
  { final, supertypes, ty } -> do
    DBuffer.addByte b (if final then 0x4F else 0x50)
    write_vec b (map (write_u32 b) supertypes)
    write_comp_type b ty

write_rec_type :: DBuffer -> S.RecType -> Effect Unit
write_rec_type b = case _ of
  [] -> pure unit
  [ ty ] -> write_sub_type b ty
  tys -> do
    DBuffer.addByte b 0x4E
    write_vec b (map (write_sub_type b) tys)

write_type_section :: DBuffer -> Array S.RecType -> Effect Unit
write_type_section b types = write_section b 1 do
  write_vec b (map (write_rec_type b) types)

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
  S.I32TruncF32_s ->
    DBuffer.addByte b 0xA8
  S.I32TruncF32_u ->
    DBuffer.addByte b 0xA9
  S.I32ReinterpretF32 ->
    DBuffer.addByte b 0xBC
  S.F32Const n -> do
    DBuffer.addByte b 0x43
    float32 b n
  S.F32Eq ->
    DBuffer.addByte b 0x5B
  S.F32Neq ->
    DBuffer.addByte b 0x5C
  S.F32Lt ->
    DBuffer.addByte b 0x5D
  S.F32Gt ->
    DBuffer.addByte b 0x5E
  S.F32Ge ->
    DBuffer.addByte b 0x5F
  S.F32Le ->
    DBuffer.addByte b 0x60
  S.F32Abs ->
    DBuffer.addByte b 0x8B
  S.F32Neg ->
    DBuffer.addByte b 0x8C
  S.F32Ceil ->
    DBuffer.addByte b 0x8D
  S.F32Floor ->
    DBuffer.addByte b 0x8E
  S.F32Trunc ->
    DBuffer.addByte b 0x8F
  S.F32Nearest ->
    DBuffer.addByte b 0x90
  S.F32Sqrt ->
    DBuffer.addByte b 0x91
  S.F32Add ->
    DBuffer.addByte b 0x92
  S.F32Sub ->
    DBuffer.addByte b 0x93
  S.F32Mul ->
    DBuffer.addByte b 0x94
  S.F32Div ->
    DBuffer.addByte b 0x95
  S.F32Min ->
    DBuffer.addByte b 0x96
  S.F32Max ->
    DBuffer.addByte b 0x97
  S.F32Copysign ->
    DBuffer.addByte b 0x98
  S.F32ConvertI32_s ->
    DBuffer.addByte b 0xB2
  S.F32ConvertI32_u ->
    DBuffer.addByte b 0xB3
  S.RefNull t -> do
    DBuffer.addByte b 0xD0
    write_heap_type b t
  S.RefIsNull ->
    DBuffer.addByte b 0xD1
  S.RefFunc idx -> do
    DBuffer.addByte b 0xD2
    write_u32 b idx
  S.ArrayNew x -> do
    DBuffer.addByte b 0xFB
    write_u32 b 6
    write_u32 b x
  S.ArrayNewDefault x -> do
    DBuffer.addByte b 0xFB
    write_u32 b 7
    write_u32 b x
  S.ArrayNewFixed x y -> do
    DBuffer.addByte b 0xFB
    write_u32 b 8
    write_u32 b x
    write_u32 b y
  S.ArrayNewData x y -> do
    DBuffer.addByte b 0xFB
    write_u32 b 9
    write_u32 b x
    write_u32 b y
  S.ArrayNewElem x y -> do
    DBuffer.addByte b 0xFB
    write_u32 b 10
    write_u32 b x
    write_u32 b y
  S.ArrayGet x -> do
    DBuffer.addByte b 0xFB
    write_u32 b 11
    write_u32 b x
  S.ArrayGet_s x -> do
    DBuffer.addByte b 0xFB
    write_u32 b 12
    write_u32 b x
  S.ArrayGet_u x -> do
    DBuffer.addByte b 0xFB
    write_u32 b 13
    write_u32 b x
  S.ArraySet x -> do
    DBuffer.addByte b 0xFB
    write_u32 b 14
    write_u32 b x
  S.ArrayLen -> do
    DBuffer.addByte b 0xFB
    write_u32 b 15
  S.ArrayFill x -> do
    DBuffer.addByte b 0xFB
    write_u32 b 16
    write_u32 b x
  S.ArrayCopy x y -> do
    DBuffer.addByte b 0xFB
    write_u32 b 17
    write_u32 b x
    write_u32 b y
  S.ArrayInitData x y -> do
    DBuffer.addByte b 0xFB
    write_u32 b 18
    write_u32 b x
    write_u32 b y
  S.ArrayInitElem x y -> do
    DBuffer.addByte b 0xFB
    write_u32 b 19
    write_u32 b x
    write_u32 b y
  S.Drop ->
    DBuffer.addByte b 0x1A
  S.Select m_tys -> case m_tys of
    Nothing ->
      DBuffer.addByte b 0x1B
    Just tys -> do
      DBuffer.addByte b 0x1C
      write_vec b (map (write_value_type b) tys)
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
  S.NullFuncRef -> signed_leb128 b (-0x0d)
  S.FuncRef -> DBuffer.addByte b 0x70
  S.ExternRef -> DBuffer.addByte b 0x6F
  S.HeapTypeRef nullable t -> do
    DBuffer.addByte b (if nullable then 0x63 else 0x64)
    write_heap_type b t

write_heap_type :: DBuffer -> S.HeapType -> Effect Unit
write_heap_type b = case _ of
  S.IndexHt ix -> write_s33 b ix

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

elem_index :: S.Expr -> Maybe S.FuncIdx
elem_index = case _ of
  [ S.RefFunc x ] -> Just x
  _ -> Nothing

write_elem :: DBuffer -> S.Elem -> Effect Unit
write_elem b { type: ty, init, mode }
  | ty == S.FuncRef
  , Just idxs <- traverse elem_index init = case mode of
      S.ElemPassive -> do
        write_u32 b 0x01
        DBuffer.addByte b 0x00 -- S.FuncRef
        write_vec b (map (write_u32 b) idxs)
      S.ElemActive { table, offset }
        | table == 0 && ty == S.FuncRef -> do
            write_u32 b 0x00
            write_expr b offset
            write_vec b (map (write_u32 b) idxs)
        | otherwise -> do
            write_u32 b 0x02
            write_u32 b table
            write_expr b offset
            DBuffer.addByte b 0x00 -- S.FuncRef
            write_vec b (map (write_u32 b) idxs)
      S.ElemDeclarative -> do
        write_u32 b 0x03
        DBuffer.addByte b 0x00 -- S.FuncRef
        write_vec b (map (write_u32 b) idxs)
  | otherwise = case mode of
      S.ElemPassive -> do
        write_u32 b 0x05
        DBuffer.addByte b 0x00 -- S.FuncRef
        write_vec b (map (write_expr b) init)
      S.ElemActive { table, offset }
        | table == 0 && ty == S.FuncRef -> do
            write_u32 b 0x04
            write_expr b offset
            write_vec b (map (write_expr b) init)
        | otherwise -> do
            write_u32 b 0x06
            write_u32 b table
            write_expr b offset
            DBuffer.addByte b 0x00 -- S.FuncRef
            write_vec b (map (write_expr b) init)
      S.ElemDeclarative -> do
        write_u32 b 0x07
        DBuffer.addByte b 0x00 -- S.FuncRef
        write_vec b (map (write_expr b) init)

write_elem_section :: DBuffer -> Array S.Elem -> Effect Unit
write_elem_section b elems = write_section b 9 do
  write_vec b (map (write_elem b) elems)

write_locals :: DBuffer -> Array S.ValType -> Array (Effect Unit)
write_locals b locals = do
  let grouped = Array.group locals
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
  DataPassive -> do
    DBuffer.addByte b 0x01
    write_vec b (map (DBuffer.addByte b) dat.init)
  DataActive { memory, offset }
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
