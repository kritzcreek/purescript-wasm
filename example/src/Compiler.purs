module Compiler (compileProgram) where

import Prelude

import Ast as Ast
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Rename (Var(..))
import Types as Types
import Wasm.Syntax as S
import WasmBuilder (bodyBuild)
import WasmBuilder as Builder

type Builder = Builder.Builder Var
type BodyBuilder = Builder.BodyBuilder Var

type CFunc = Ast.Func Types.Ty Var
type CExpr = Ast.Expr Types.Ty Var
type CDecl = Ast.Decl Types.Ty Var
type CToplevel = Ast.Toplevel Types.Ty Var

type CProgram = Ast.Program Types.Ty Var

type FillFunc =
  { fill :: Array S.ValType -> S.Expr -> Builder Unit
  , func :: CFunc
  }

i32 :: S.ValType
i32 = S.NumType S.I32

f32 :: S.ValType
f32 = S.NumType S.F32

typeOf :: CExpr -> S.ValType
typeOf e = case Types.typeOf e of
  Types.TyI32 -> i32
  Types.TyF32 -> f32
  Types.TyBool -> i32
  Types.TyUnit -> i32
  Types.TyArray _ -> unsafeCrashWith "Can't produce a ValType for ArrayTy"

convertValTy :: Ast.ValTy -> S.ValType
convertValTy = case _ of
  Ast.TyI32 -> i32
  Ast.TyF32 -> f32
  Ast.TyBool -> i32
  Ast.TyUnit -> i32
  Ast.TyArray _ -> unsafeCrashWith "Can't produce a ValType for ArrayTy"

convertFuncTy :: Ast.FuncTy -> S.FuncType
convertFuncTy = case _ of
  Ast.FuncTy arguments result ->
    { arguments: map convertValTy arguments
    , results: [ convertValTy result ]
    }

compileProgram :: CProgram -> S.Module
compileProgram toplevels = Builder.build' do
  fills <- traverse declareToplevel toplevels
  traverse_ implFunc (Array.catMaybes fills)

declareToplevel :: CToplevel -> Builder (Maybe FillFunc)
declareToplevel = case _ of
  Ast.TopLet name init -> do
    _ <- Builder.declareGlobal name { mutability: S.Var, type: typeOf init } (compileConst init)
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

compileConst :: forall a b. Ast.Expr a b -> S.Expr
compileConst e = unsafePartial case e.expr of
  Ast.LitE (Ast.IntLit x) -> [ S.I32Const x ]
  Ast.LitE (Ast.FloatLit x) -> [ S.F32Const x ]

compileOp :: Types.Ty -> Ast.Op -> S.Instruction
compileOp = case _, _ of
  Types.TyBool, Ast.Eq -> S.I32Eq

  Types.TyI32, Ast.Add -> S.I32Add
  Types.TyI32, Ast.Sub -> S.I32Sub
  Types.TyI32, Ast.Mul -> S.I32Mul
  Types.TyI32, Ast.Div -> S.I32Div_s
  Types.TyI32, Ast.Lt -> S.I32Lt_s
  Types.TyI32, Ast.Gt -> S.I32Gt_s
  Types.TyI32, Ast.Lte -> S.I32Le_s
  Types.TyI32, Ast.Gte -> S.I32Ge_s
  Types.TyI32, Ast.Eq -> S.I32Eq

  Types.TyF32, Ast.Add -> S.F32Add
  Types.TyF32, Ast.Sub -> S.F32Sub
  Types.TyF32, Ast.Mul -> S.F32Mul
  Types.TyF32, Ast.Div -> S.F32Div
  Types.TyF32, Ast.Lt -> S.F32Lt
  Types.TyF32, Ast.Gt -> S.F32Gt
  Types.TyF32, Ast.Lte -> S.F32Le
  Types.TyF32, Ast.Gte -> S.F32Ge
  Types.TyF32, Ast.Eq -> S.F32Eq
  t, o ->
    unsafeCrashWith ("no instruction for operand: " <> show o <> " at type: " <> show t)

compileLit :: Ast.Lit -> S.Expr
compileLit = case _ of
  Ast.IntLit x -> [ S.I32Const x ]
  Ast.FloatLit x -> [ S.F32Const x ]
  Ast.BoolLit b -> [ if b then S.I32Const 1 else S.I32Const 0 ]

compileExpr :: CExpr -> BodyBuilder S.Expr
compileExpr expr = case expr.expr of
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
    in l' <> r' <> [ compileOp (Types.typeOf l) op ]
  Ast.IfE cond t e -> ado
    cond' <- compileExpr cond
    t' <- compileExpr t
    e' <- compileExpr e
    in cond' <> [ S.If (S.BlockValType (Just (typeOf t))) t' e' ]
  Ast.CallE fn args -> do
    args' <- traverse compileExpr args
    call <- Builder.liftBuilder do
      Builder.callImport fn >>= case _ of
        Just importCall -> pure importCall
        Nothing -> Builder.callFunc fn
    pure (Array.fold args' <> [ call ])
  Ast.BlockE body -> compileBlock body
  Ast.ArrayE elements -> do
    let
      elTy = case expr.note of
        Types.TyArray t -> t
        _ -> unsafeCrashWith "non-array type inferred for array literal"
    compileArray elTy elements

compileArray :: Types.Ty -> Array CExpr -> BodyBuilder S.Expr
compileArray _elTy _elements = unsafeCrashWith "No codegen for arrays yet"

compileBlock
  :: Array CDecl
  -> BodyBuilder S.Expr
compileBlock decls = do
  case Array.unsnoc decls of
    Just { init, last: Ast.ExprD expr } -> do
      instrs <- traverse go init
      result <- compileExpr expr
      pure (Array.concat instrs <> result)
    _ ->
      unsafeCrashWith "block must end in an expression."
  where
  go :: CDecl -> BodyBuilder S.Expr
  go = case _ of
    Ast.ExprD expr -> do
      is <- compileExpr expr
      pure (is <> [ S.Drop ])
    Ast.LetD n e -> do
      var <- Builder.newLocal n (typeOf e)
      is <- compileExpr e
      pure (is <> [ S.LocalSet var ])
    Ast.SetD n e -> do
      case n of
        GlobalV _ -> do
          ix <- Builder.liftBuilder (Builder.lookupGlobal n)
          is <- compileExpr e
          pure (is <> [ S.GlobalSet ix ])
        _ -> unsafeCrashWith "Unknown set target"

declareFunc :: CFunc -> Builder FillFunc
declareFunc func = do
  fill <- Builder.declareFunc
    func.name
    { arguments: map (\p -> convertValTy p.ty) func.params
    , results: [ convertValTy func.returnTy ]
    }
  pure { fill, func }

implFunc :: FillFunc -> Builder Unit
implFunc { fill, func } = do
  let paramTys = map (\v -> Tuple v.name (convertValTy v.ty)) func.params
  fnBody <- bodyBuild paramTys (compileExpr func.body)
  fill fnBody.locals fnBody.result
