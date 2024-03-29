-- | Defines the Abstract Syntax of WebAssembly as PureScript data types
-- |
-- | Follows: https://webassembly.github.io/spec/core/syntax/index.html
module Wasm.Syntax where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String as String

data RefType
  = NullFuncRef
  | FuncRef
  | ExternRef
  | HeapTypeRef Boolean HeapType

derive instance eqRefType :: Eq RefType
derive instance ordRefType :: Ord RefType
instance showRefType :: Show RefType where
  show = case _ of
    NullFuncRef -> "nullfuncref"
    FuncRef -> "funcref"
    ExternRef -> "externref"
    HeapTypeRef null ix -> "ref " <> (if null then "null " else "") <> show ix

data HeapType = IndexHt TypeIdx

derive instance Eq HeapType
derive instance Ord HeapType
instance Show HeapType where
  show = case _ of
    IndexHt ix -> show ix

data PackedType = I8 | I16

derive instance Eq PackedType
derive instance Ord PackedType
instance Show PackedType where
  show = case _ of
    I8 -> "i8"
    I16 -> "i16"

data StorageType = StorageVal ValType | StoragePacked PackedType

derive instance Eq StorageType
derive instance Ord StorageType
instance Show StorageType where
  show = case _ of
    StorageVal t -> show t
    StoragePacked t -> show t

type FieldType =
  { mutability :: Mutability
  , ty :: StorageType
  }

type StructType = Array FieldType
type ArrayType = FieldType

data CompositeType = CompFunc FuncType | CompStruct StructType | CompArray ArrayType

derive instance Eq CompositeType
derive instance Ord CompositeType
instance Show CompositeType where
  show = case _ of
    CompFunc t -> show t
    CompStruct t -> "(struct " <> show t <> ")"
    CompArray t -> "(array " <> show t <> ")"

type RecType = Array SubType

type SubType =
  { final :: Boolean
  , supertypes :: Array TypeIdx
  , ty :: CompositeType
  }

data NumType = I32 | I64 | F32 | F64

derive instance eqNumType :: Eq NumType
derive instance ordNumType :: Ord NumType
instance showNumType :: Show NumType where
  show = case _ of
    I32 -> "i32"
    I64 -> "i64"
    F32 -> "f64"
    F64 -> "f64"

data ValType = NumType NumType | RefType RefType

derive instance eqValType :: Eq ValType
derive instance ordValType :: Ord ValType
instance showValType :: Show ValType where
  show = case _ of
    NumType n -> show n
    RefType r -> show r

type ResultType = Array ValType

type FuncType =
  { arguments :: ResultType
  , results :: ResultType
  }

type TypeIdx = Int
type LocalIdx = Int
type GlobalIdx = Int
type LabelIdx = Int
type FuncIdx = Int
type MemoryIdx = Int
type TableIdx = Int
type DataIdx = Int
type ElemIdx = Int
type FieldIdx = Int

type Expr = Array Instruction

type MemArg = { offset :: Int, align :: Int }

data Instruction
  = I32Const Int
  | I32Clz
  | I32Ctz
  | I32Popcnt
  | I32Add
  | I32Sub
  | I32Mul
  | I32Div_u
  | I32Div_s
  | I32Rem_u
  | I32Rem_s
  | I32And
  | I32Or
  | I32Xor
  | I32Shl
  | I32Shr_u
  | I32Shr_s
  | I32Rotl
  | I32Rotr
  | I32Eqz
  | I32Eq
  | I32Ne
  | I32Lt_u
  | I32Lt_s
  | I32Gt_u
  | I32Gt_s
  | I32Le_u
  | I32Le_s
  | I32Ge_u
  | I32Ge_s
  | I32Extend8_s
  | I32Extend16_s
  | I32Wrap_i64
  | I32TruncF32_s
  | I32TruncF32_u
  | I32ReinterpretF32

  -- Floating Point Instructions

  | F32Const Number
  | F32Eq
  | F32Ne
  | F32Lt
  | F32Gt
  | F32Ge
  | F32Le
  | F32Abs
  | F32Neg
  | F32Ceil
  | F32Floor
  | F32Trunc
  | F32Nearest
  | F32Sqrt
  | F32Add
  | F32Sub
  | F32Mul
  | F32Div
  | F32Min
  | F32Max
  | F32Copysign
  | F32ConvertI32_s
  | F32ConvertI32_u

  -- Reference instructions
  | RefNull HeapType
  | RefIsNull
  | RefFunc FuncIdx

  -- Aggregate Instructions
  | ArrayNew TypeIdx
  | ArrayNewFixed TypeIdx Int
  | ArrayNewDefault TypeIdx
  | ArrayNewData TypeIdx DataIdx
  | ArrayNewElem TypeIdx ElemIdx
  | ArrayGet TypeIdx
  | ArrayGet_s TypeIdx
  | ArrayGet_u TypeIdx
  | ArraySet TypeIdx
  | ArrayLen
  | ArrayFill TypeIdx
  | ArrayCopy TypeIdx TypeIdx
  | ArrayInitData TypeIdx DataIdx
  | ArrayInitElem TypeIdx ElemIdx

  | StructNew TypeIdx
  | StructNewDefault TypeIdx
  | StructGet TypeIdx FieldIdx
  | StructGet_s TypeIdx FieldIdx
  | StructGet_u TypeIdx FieldIdx
  | StructSet TypeIdx FieldIdx

  -- Parametric Instructions
  | Drop
  | Select (Maybe (Array ValType))

  -- Variable Instructions
  | LocalGet LocalIdx
  | LocalSet LocalIdx
  | LocalTee LocalIdx
  | GlobalGet GlobalIdx
  | GlobalSet GlobalIdx

  -- Memory Instructions
  | I32Load MemArg
  | I32Load8_s MemArg
  | I32Load8_u MemArg
  | I32Load16_s MemArg
  | I32Load16_u MemArg
  | I32Store MemArg
  | I32Store8 MemArg
  | I32Store16 MemArg
  | MemorySize
  | MemoryGrow
  | MemoryFill
  | MemoryCopy
  | MemoryInit DataIdx
  | DataDrop DataIdx

  -- Control Instructions
  | Nop
  | Unreachable
  | Block BlockType (Array Instruction)
  | Loop BlockType (Array Instruction)
  | If BlockType (Array Instruction) (Array Instruction)
  | Br LabelIdx
  | Br_if LabelIdx
  | Br_table (Array LabelIdx) LabelIdx
  | Return
  | Call FuncIdx
  | Call_Indirect TypeIdx

derive instance Eq Instruction
derive instance Ord Instruction
instance Show Instruction where
  show = case _ of
    I32Const x -> "i32.const " <> show x
    I32Clz -> "i32.clz"
    I32Ctz -> "i32.ctz"
    I32Popcnt -> "i32.popcnt"
    I32Add -> "i32.add"
    I32Sub -> "i32.sub"
    I32Mul -> "i32.mul"
    I32Div_u -> "i32.div_u"
    I32Div_s -> "i32.div_s"
    I32Rem_u -> "i32.rem_u"
    I32Rem_s -> "i32.rem_s"
    I32And -> "i32.and"
    I32Or -> "i32.or"
    I32Xor -> "i32.xor"
    I32Shl -> "i32.shl"
    I32Shr_u -> "i32.shr_u"
    I32Shr_s -> "i32.shr_s"
    I32Rotl -> "i32.rotl"
    I32Rotr -> "i32.rotr"
    I32Eqz -> "i32.eqz"
    I32Eq -> "i32.eq"
    I32Ne -> "i32.ne"
    I32Lt_u -> "i32.lt_u"
    I32Lt_s -> "i32.lt_s"
    I32Gt_u -> "i32.gt_u"
    I32Gt_s -> "i32.gt_s"
    I32Le_u -> "i32.le_u"
    I32Le_s -> "i32.le_s"
    I32Ge_u -> "i32.ge_u"
    I32Ge_s -> "i32.ge_s"
    I32Extend8_s -> "i32.extend8_s"
    I32Extend16_s -> "i32.extend16_s"
    I32Wrap_i64 -> "i32.wrap_i64"
    I32TruncF32_s -> "i32.trunc_f32_s"
    I32TruncF32_u -> "i32.trunc_f32_u"
    I32ReinterpretF32 -> "i32.reinterpret_f32"
    F32Const x -> "f32.const " <> show x
    F32Eq -> "f32.eq"
    F32Ne -> "f32.ne"
    F32Lt -> "f32.lt"
    F32Gt -> "f32.gt"
    F32Ge -> "f32.ge"
    F32Le -> "f32.le"
    F32Abs -> "f32.abs"
    F32Neg -> "f32.neg"
    F32Ceil -> "f32.ceil"
    F32Floor -> "f32.floor"
    F32Trunc -> "f32.trunc"
    F32Nearest -> "f32.nearest"
    F32Sqrt -> "f32.sqrt"
    F32Add -> "f32.add"
    F32Sub -> "f32.sub"
    F32Mul -> "f32.mul"
    F32Div -> "f32.div"
    F32Min -> "f32.min"
    F32Max -> "f32.max"
    F32Copysign -> "f32.copysign"
    F32ConvertI32_s -> "f32.convert_i32_s"
    F32ConvertI32_u -> "f32.convert_i32_u"
    RefNull x -> "ref.null " <> show x
    RefIsNull -> "ref.is_null"
    RefFunc x -> "ref.func " <> show x
    ArrayNew x -> "array.new " <> show x
    ArrayNewFixed x y -> "array.new_fixed " <> show x <> " " <> show y
    ArrayNewDefault x -> "array.new_default " <> show x
    ArrayNewData x y -> "array.new_data " <> show x <> " " <> show y
    ArrayNewElem x y -> "array.new_elem " <> show x <> " " <> show y
    ArrayGet x -> "array.get " <> show x
    ArrayGet_s x -> "array.get_s " <> show x
    ArrayGet_u x -> "array.get_u " <> show x
    ArraySet x -> "array.set " <> show x
    ArrayLen -> "array.len"
    ArrayFill x -> "array.fill " <> show x
    ArrayCopy x y -> "array.copy " <> show x <> " " <> show y
    ArrayInitData x y -> "array.init_data " <> show x <> " " <> show y
    ArrayInitElem x y -> "array.init_elem " <> show x <> " " <> show y
    StructNew x -> "struct.new " <> show x
    StructNewDefault x -> "struct.new_default " <> show x
    StructGet x y -> "struct.get " <> show x <> " " <> show y
    StructGet_s x y -> "struct.get_s " <> show x <> " " <> show y
    StructGet_u x y -> "struct.get_u " <> show x <> " " <> show y
    StructSet x y -> "struct.set " <> show x <> " " <> show y
    Drop -> "drop"
    Select Nothing -> "select"
    Select (Just tys) -> "select " <> String.joinWith " " (map show tys)
    LocalGet x -> "local.get " <> show x
    LocalSet x -> "local.set " <> show x
    LocalTee x -> "local.tee " <> show x
    GlobalGet x -> "global.get " <> show x
    GlobalSet x -> "global.set " <> show x
    I32Load x -> "i32.load" <> show x
    I32Load8_s x -> "i32.load8_s " <> show x
    I32Load8_u x -> "i32.load8_u " <> show x
    I32Load16_s x -> "i32.load16_s " <> show x
    I32Load16_u x -> "i32.load16_u " <> show x
    I32Store x -> "i32.store " <> show x
    I32Store8 x -> "i32.store8 " <> show x
    I32Store16 x -> "i32.store16 " <> show x
    MemorySize -> "memory.size"
    MemoryGrow -> "memory.grow"
    MemoryFill -> "memory.fill"
    MemoryCopy -> "memory.copy"
    MemoryInit x -> "memory.init " <> show x
    DataDrop x -> "data.drop " <> show x
    Nop -> "nop"
    Unreachable -> "unreachable"
    Block x y -> "block " <> show x <> " " <> show y
    Loop x y -> "loop " <> show x <> " " <> show y
    If x y z -> "if " <> show x <> " " <> show y <> " " <> show z
    Br x -> "br " <> show x
    Br_if x -> "br_if" <> show x
    Br_table x y -> "br_table " <> show x <> " " <> show y
    Return -> "return"
    Call x -> "call " <> show x
    Call_Indirect x -> "call_indirect " <> show x

data BlockType = BlockTypeIdx TypeIdx | BlockValType (Maybe ValType)

derive instance Eq BlockType
derive instance Ord BlockType
instance showBlockType :: Show BlockType where
  show = case _ of
    BlockTypeIdx x -> show x
    BlockValType x -> show x

type Func =
  { type :: TypeIdx
  , locals :: Array ValType
  , body :: Expr
  }

type Limits = { min :: Int, max :: Maybe Int }

type TableType =
  { limits :: Limits
  , elemtype :: RefType
  }

type Table =
  { type :: TableType
  }

type MemoryType = Limits

type Memory =
  { type :: MemoryType
  }

data Mutability = Const | Var

derive instance Eq Mutability
derive instance Ord Mutability
instance showMutability :: Show Mutability where
  show = case _ of
    Const -> "const"
    Var -> "var"

type GlobalType =
  { mutability :: Mutability
  , type :: ValType
  }

type Global =
  { type :: GlobalType
  , init :: Expr
  }

type Elem =
  { type :: RefType
  , init :: Array Expr
  , mode :: ElemMode
  }

data ElemMode
  = ElemPassive
  | ElemDeclarative
  | ElemActive { table :: TableIdx, offset :: Expr }

derive instance Eq ElemMode
derive instance Ord ElemMode
instance Show ElemMode where
  show = case _ of
    ElemPassive -> "passive"
    ElemDeclarative -> "declarative"
    ElemActive { table, offset } -> "active " <> show table <> " " <> show offset

type Byte = Int
type Data =
  { mode :: DataMode
  , init :: Array Byte
  }

data DataMode
  = DataPassive
  | DataActive { offset :: Expr, memory :: MemoryIdx }

derive instance Eq DataMode
derive instance Ord DataMode
instance Show DataMode where
  show = case _ of
    DataPassive -> "passive"
    DataActive { offset, memory } -> "active " <> show offset <> " " <> show memory

type Name = String

data ImportDesc
  = ImportFunc TypeIdx
  | ImportTable TableType
  | ImportMemory MemoryType
  | ImportGlobal GlobalType

instance showImportDesc :: Show ImportDesc where
  show = case _ of
    ImportFunc x -> "ImportFunc " <> show x
    ImportTable x -> "ImportTable " <> show x
    ImportMemory x -> "ImportMemory " <> show x
    ImportGlobal x -> "ImportGlobal " <> show x

type Import =
  { module :: Name
  , name :: Name
  , desc :: ImportDesc
  }

data ExportDesc
  = ExportFunc FuncIdx
  | ExportTable TableIdx
  | ExportMemory MemoryIdx
  | ExportGlobal GlobalIdx

instance showExportDesc :: Show ExportDesc where
  show = case _ of
    ExportFunc x -> "ExportFunc " <> show x
    ExportTable x -> "ExportTable " <> show x
    ExportMemory x -> "ExportMemory " <> show x
    ExportGlobal x -> "ExportGlobal " <> show x

type Export =
  { name :: Name
  , desc :: ExportDesc
  }

type Module =
  { types :: Array RecType
  , funcs :: Array Func
  , tables :: Array Table
  , memories :: Array Memory
  , globals :: Array Global
  , elem :: Array Elem
  , data :: Array Data
  , start :: Maybe FuncIdx
  , imports :: Array Import
  , exports :: Array Export
  }

emptyModule :: Module
emptyModule =
  { types: []
  , funcs: []
  , tables: []
  , memories: []
  , globals: []
  , elem: []
  , data: []
  , start: Nothing
  , imports: []
  , exports: []
  }
