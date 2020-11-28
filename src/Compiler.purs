module Compiler where

import Prelude

import AST as AST
import Data.Array as Array
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (traverse, traverse_)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Wasm.Builder as Builder
import Wasm.Syntax as S

-- fn fib(x) {
--   if x < 2 {
--     1
--   } else {
--     add(fib(x - 1), fib(x - 2))
--   }
-- }

-- fn main() {
--   fib(10)
-- }

type Builder = Builder.Builder String

compileFuncs :: Array (AST.Func String) -> S.Module
compileFuncs funcs = Builder.build' do
  fills <- traverse declareFunc funcs
  traverse_ implFunc fills
  Builder.declareExport "main" "main"

compileOp :: AST.Op -> S.Instruction
compileOp = case _ of
  AST.Add -> S.I32Add
  AST.Sub -> S.I32Sub
  AST.Mul -> S.I32Mul
  AST.Div -> S.I32Div_s
  AST.Lt -> S.I32Lt_s
  AST.Gt -> S.I32Gt_s
  AST.Lte -> S.I32Le_s
  AST.Gte -> S.I32Ge_s
  AST.Eq -> S.I32Eq

compileExpr :: (String -> S.LocalIdx) -> AST.Expr String -> Builder (Array S.Instruction)
compileExpr findParam = case _ of
  AST.IntLit x -> pure [S.I32Const x]
  AST.BoolLit b -> pure [if b then S.I32Const 1 else S.I32Const 0]
  AST.Var x -> pure [S.LocalGet (findParam x)]
  AST.BinOp op l r -> ado
    l' <- compileExpr findParam l
    r' <- compileExpr findParam r
    in l' <> r' <> [compileOp op]
  AST.If cond t e -> ado
    cond' <- compileExpr findParam cond
    t' <- compileExpr findParam t
    e' <- compileExpr findParam e
    in cond' <> [S.If (S.BlockValType (Just S.I32)) t' e']
  AST.Call func args -> do
    args' <- traverse (compileExpr findParam) args
    call <- Builder.callFunc func
    pure (Array.fold args' <> [call])

compileBody :: (String -> S.LocalIdx) -> Array (AST.Decl String) -> Builder (Array S.Instruction)
compileBody findParam decls = case decls of
  [ AST.ExprD expr ] -> compileExpr findParam expr
  _ -> unsafeCrashWith "Multiple decls not supported"

declareFunc ::
  AST.Func String ->
  Builder
    { fill :: S.Expr -> Builder Unit
    , func :: AST.Func String
    }
declareFunc func@(AST.Func name params body) = do
  fill <- Builder.declareFunc
    name
    { arguments: Array.replicate (Array.length params) S.I32
    , results: [S.I32]
    }
    []
  pure { fill, func }

implFunc ::
  { fill :: S.Expr -> Builder Unit
  , func :: AST.Func String
  } ->
  Builder Unit
implFunc { fill, func: AST.Func _ params body } =
  fill =<< compileBody (\x -> unsafePartial fromJust (Array.elemIndex x params)) body
