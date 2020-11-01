-- | Defines the Abstract Syntax of WebAssembly as PureScript data types
module Wasm.Syntax where

import Data.Maybe (Maybe)

data ValType = I32 | I64 | F32 | F64

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
  | I32Store MemArg
  | I32Load8_u MemArg
  | I32Load8_s MemArg
  | I32Load16_u MemArg
  | I32Load16_s MemArg
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

data BlockType = BlockTypeIdx TypeIdx | BlockValType (Maybe ValType)

type Func =
  { type :: TypeIdx
  , locals :: Array ValType
  , body :: Expr
  }

type Limits = { min :: Int, max :: Maybe Int}
data ElemType = FuncRef

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
