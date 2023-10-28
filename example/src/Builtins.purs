module Builtins (all, find, Fn) where

import Prelude

import Ast (FuncTy(..), ValTy(..))
import Data.Array as Array
import Data.Maybe (Maybe)
import Wasm.Syntax as S

type Fn =
  { name :: String
  , ty :: FuncTy String
  , instr :: S.Instruction
  }

find :: String -> Maybe Fn
find n = Array.find (\bi -> bi.name == n) all

all :: Array Fn
all =
  [ f32_neg
  , f32_abs
  , f32_ceil
  , f32_floor
  , f32_trunc
  , f32_nearest
  , f32_sqrt
  , f32_copysign
  , f32_convert_i32_s
  , f32_convert_i32_u
  , f32_min
  , f32_max

  , i32_clz
  , i32_ctz
  , i32_popcnt
  , i32_rem_s
  , i32_shl
  , i32_shr_s
  , i32_rotl
  , i32_rotr
  , i32_trunc_f32_s
  , i32_reinterpret_f32
  ]

f32_func_unary :: String -> S.Instruction -> Fn
f32_func_unary name instr =
  { name
  , ty: FuncTy [ TyF32 ] TyF32
  , instr
  }

f32_func_binary :: String -> S.Instruction -> Fn
f32_func_binary name instr =
  { name
  , ty: FuncTy [ TyF32, TyF32 ] TyF32
  , instr
  }

f32_neg :: Fn
f32_neg = f32_func_unary "f32_neg" S.F32Neg

f32_abs :: Fn
f32_abs = f32_func_unary "f32_abs" S.F32Abs

f32_ceil :: Fn
f32_ceil = f32_func_unary "f32_ceil" S.F32Ceil

f32_floor :: Fn
f32_floor = f32_func_unary "f32_floor" S.F32Floor

f32_trunc :: Fn
f32_trunc = f32_func_unary "f32_trunc" S.F32Trunc

f32_nearest :: Fn
f32_nearest = f32_func_unary "f32_nearest" S.F32Nearest

f32_sqrt :: Fn
f32_sqrt = f32_func_unary "f32_sqrt" S.F32Sqrt

f32_copysign :: Fn
f32_copysign = f32_func_unary "f32_copysign" S.F32Copysign

f32_convert_i32_s :: Fn
f32_convert_i32_s =
  { name: "f32_convert_i32_s"
  , ty: FuncTy [ TyF32 ] TyI32
  , instr: S.F32ConvertI32_s
  }

f32_convert_i32_u :: Fn
f32_convert_i32_u =
  { name: "f32_convert_i32_u"
  , ty: FuncTy [ TyF32 ] TyI32
  , instr: S.F32ConvertI32_u
  }

f32_min :: Fn
f32_min = f32_func_binary "f32_min" S.F32Min

f32_max :: Fn
f32_max = f32_func_binary "f32_max" S.F32Max

i32_func_unary :: String -> S.Instruction -> Fn
i32_func_unary name instr =
  { name
  , ty: FuncTy [ TyI32 ] TyI32
  , instr
  }

i32_func_binary :: String -> S.Instruction -> Fn
i32_func_binary name instr =
  { name
  , ty: FuncTy [ TyI32, TyI32 ] TyI32
  , instr
  }

i32_clz :: Fn
i32_clz = i32_func_unary "i32_clz" S.I32Clz

i32_ctz :: Fn
i32_ctz = i32_func_unary "i32_ctz" S.I32Ctz

i32_popcnt :: Fn
i32_popcnt = i32_func_unary "i32_popcnt" S.I32Popcnt

i32_rem_s :: Fn
i32_rem_s = i32_func_binary "i32_rem_s" S.I32Rem_s

i32_shl :: Fn
i32_shl = i32_func_binary "i32_shl" S.I32Shl

i32_shr_s :: Fn
i32_shr_s = i32_func_binary "i32_shr_s" S.I32Shr_s

i32_rotl :: Fn
i32_rotl = i32_func_unary "i32_rotl" S.I32Rotl

i32_rotr :: Fn
i32_rotr = i32_func_unary "i32_rotr" S.I32Rotr

i32_trunc_f32_s :: Fn
i32_trunc_f32_s =
  { name: "i32_trunc_f32_s"
  , ty: FuncTy [ TyI32 ] TyF32
  , instr: S.I32TruncF32_s
  }

i32_reinterpret_f32 :: Fn
i32_reinterpret_f32 =
  { name: "i32_reinterpret_f32"
  , ty: FuncTy [ TyI32 ] TyF32
  , instr: S.I32ReinterpretF32
  }
