module Compiler (compileProgram) where

import Prelude

import Ast (Program)
import Ast as Ast
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Rename (Var(..))
import Wasm.Syntax as S
import WasmBuilder (bodyBuild)
import WasmBuilder as Builder

type Builder = Builder.Builder Var
type BodyBuilder = Builder.BodyBuilder Var

type FillFunc =
  { fill :: Array S.ValType -> S.Expr -> Builder Unit
  , func :: Ast.Func Var
  }

i32 :: S.ValType
i32 = S.NumType S.I32

convertValTy :: Ast.ValTy -> S.ValType
convertValTy = case _ of
  Ast.TyI32 -> i32
  Ast.TyBool -> i32
  Ast.TyUnit -> i32

convertFuncTy :: Ast.FuncTy -> S.FuncType
convertFuncTy = case _ of
  Ast.FuncTy arguments result ->
    { arguments: map convertValTy arguments
    , results: [ convertValTy result ]
    }

compileProgram :: Program Var -> S.Module
compileProgram toplevels = Builder.build' do
  fills <- traverse declareToplevel toplevels
  traverse_ implFunc (Array.catMaybes fills)

declareToplevel :: Ast.Toplevel Var -> Builder (Maybe FillFunc)
declareToplevel = case _ of
  Ast.TopLet name init -> do
    _ <- Builder.declareGlobal name { mutability: S.Var, type: (S.NumType S.I32) } (compileConst init)
    pure Nothing
  Ast.TopFunc func -> do
    result <- declareFunc func
    case func.export of
      Nothing -> pure unit
      Just exportName -> Builder.declareExport func.name exportName
    pure (Just result)
  Ast.TopImport name ty externalName -> do
    tyIdx <- Builder.declareType (convertFuncTy ty)
    _ <- Builder.declareImport name "env" externalName tyIdx
    pure Nothing

compileConst :: forall a. Ast.Expr a -> S.Expr
compileConst = unsafePartial case _ of
  Ast.LitE (Ast.IntLit x) -> [ S.I32Const x ]

compileOp :: Ast.Op -> S.Instruction
compileOp = case _ of
  Ast.Add -> S.I32Add
  Ast.Sub -> S.I32Sub
  Ast.Mul -> S.I32Mul
  Ast.Div -> S.I32Div_s
  Ast.Lt -> S.I32Lt_s
  Ast.Gt -> S.I32Gt_s
  Ast.Lte -> S.I32Le_s
  Ast.Gte -> S.I32Ge_s
  Ast.Eq -> S.I32Eq

compileLit :: Ast.Lit -> Array S.Instruction
compileLit = case _ of
  Ast.IntLit x -> [ S.I32Const x ]
  Ast.BoolLit b -> [ if b then S.I32Const 1 else S.I32Const 0 ]

compileExpr :: Ast.Expr Var -> BodyBuilder (Array S.Instruction)
compileExpr = case _ of
  Ast.LitE lit -> pure (compileLit lit)
  Ast.VarE x -> case x of
    GlobalV _ -> do
      ix <- Builder.liftBuilder (Builder.lookupGlobal x)
      pure [ S.GlobalGet ix ]
    LocalV _ -> do
      ix <- Builder.getLocal x
      pure [ S.LocalGet ix ]
    FunctionV _ -> do
      unsafeCrashWith "illegal function reference in variable position"
  Ast.BinOpE op l r -> ado
    l' <- compileExpr l
    r' <- compileExpr r
    in l' <> r' <> [ compileOp op ]
  Ast.IfE cond t e -> ado
    cond' <- compileExpr cond
    t' <- compileExpr t
    e' <- compileExpr e
    in cond' <> [ S.If (S.BlockValType (Just (S.NumType S.I32))) t' e' ]
  Ast.CallE fn args -> do
    args' <- traverse compileExpr args
    call <- Builder.liftBuilder do
      Builder.callImport fn >>= case _ of
        Just importCall -> pure importCall
        Nothing -> Builder.callFunc fn
    pure (Array.fold args' <> [ call ])
  Ast.BlockE body -> compileBlock body

compileBlock
  :: Array (Ast.Decl Var)
  -> BodyBuilder (Array S.Instruction)
compileBlock decls = do
  case Array.unsnoc decls of
    Just { init, last: Ast.ExprD expr } -> do
      instrs <- traverse go init
      result <- compileExpr expr
      pure (Array.concat instrs <> result)
    _ ->
      unsafeCrashWith "block must end in an expression."
  where
  go :: Ast.Decl Var -> BodyBuilder S.Expr
  go = case _ of
    Ast.ExprD expr -> do
      is <- compileExpr expr
      pure (is <> [ S.Drop ])
    Ast.LetD n e -> do
      var <- Builder.newLocal n (S.NumType S.I32)
      is <- compileExpr e
      pure (is <> [ S.LocalSet var ])
    Ast.SetD n e -> do
      case n of
        GlobalV _ -> do
          ix <- Builder.liftBuilder (Builder.lookupGlobal n)
          is <- compileExpr e
          pure (is <> [ S.GlobalSet ix ])
        _ -> unsafeCrashWith "Unknown set target"

declareFunc :: Ast.Func Var -> Builder FillFunc
declareFunc func = do
  fill <- Builder.declareFunc
    func.name
    { arguments: map (const i32) func.params
    , results: [ i32 ]
    }
  pure { fill, func }

implFunc :: FillFunc -> Builder Unit
implFunc { fill, func } = do
  let paramTys = map (\v -> Tuple v.name (convertValTy v.ty)) func.params
  fnBody <- bodyBuild paramTys (compileExpr func.body)
  fill fnBody.locals fnBody.result
