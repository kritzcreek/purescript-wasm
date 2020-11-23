-- | Defines the Abstract Syntax of WebAssembly as PureScript data types
module Wasm.Syntax where

import Prelude

import Data.Maybe (Maybe(..))

data ValType = I32 | I64 | F32 | F64
derive instance eqValType :: Eq ValType
derive instance ordValType :: Ord ValType

instance showValType :: Show ValType where
  show = case _ of
    I32 -> "i32"
    I64 -> "i64"
    F32 -> "f64"
    F64 -> "f64"

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

type Expr = Array Instruction

type MemArg = { offset :: Int, align :: Int }

-- TODO: Add Instructions for non-I32s as well as cvtops
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

  -- Parametric Instructions
  | Drop
  | Select

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

instance showInstruction :: Show Instruction where
  show = case _ of
    I32Const x ->"i32.const " <> show x
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
    Drop -> "drop"
    Select -> "select"
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

instance showBlockType :: Show BlockType where
  show = case _ of
    BlockTypeIdx x -> show x 
    BlockValType x -> show x

type Func =
  { type :: TypeIdx
  , locals :: Array ValType
  , body :: Expr
  }

type Limits = { min :: Int, max :: Maybe Int}
data ElemType = FuncRef

instance showElemType :: Show ElemType where
  show FuncRef = "funcref"

type TableType =
  { limits :: Limits
  , elemtype :: ElemType
  }

type Table =
  { type :: TableType
  }

type MemoryType = Limits

type Memory =
  { type :: MemoryType
  }

data Mutability = Const | Var
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
  { table :: TableIdx
  , offset :: Expr
  , init :: Array FuncIdx
  }

type Byte = Int
type Data =
  { data :: MemoryIdx
  , offset :: Expr
  , init :: Array Byte
  }

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
  { types :: Array FuncType
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
  { types    : []
  , funcs    : []
  , tables   : []
  , memories : []
  , globals  : []
  , elem     : []
  , data     : []
  , start    : Nothing
  , imports  : []
  , exports  : []
  }
