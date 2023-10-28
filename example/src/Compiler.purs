module Compiler (compileProgram) where

import Prelude

import Ast as Ast
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Traversable (for, traverse, traverse_)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafeCrashWith)
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

data InitTask = FuncTask FillFunc | GlobalTask InitGlobal

type FillFunc =
  { fill :: Array S.ValType -> S.Expr -> Builder Unit
  , func :: CFunc
  }

type InitGlobal =
  { idx :: S.GlobalIdx
  , init :: CExpr
  }

i32 :: S.ValType
i32 = S.NumType S.I32

f32 :: S.ValType
f32 = S.NumType S.F32

typeOf :: CExpr -> Builder S.ValType
typeOf e = valTy (Types.typeOf e)

astValTy :: Ast.ValTy -> Builder S.ValType
astValTy t = valTy (Types.convertTy t)

valTy :: Types.Ty -> Builder S.ValType
valTy = case _ of
  Types.TyI32 -> pure i32
  Types.TyF32 -> pure f32
  Types.TyBool -> pure i32
  Types.TyUnit -> pure i32
  Types.TyArray t -> do
    elTy <- valTy t
    ix <- Builder.declareType
      [ { final: true, supertypes: [], ty: S.CompArray { mutability: S.Var, ty: S.StorageVal elTy } } ]
    pure (S.RefType (S.HeapTypeRef true (S.IndexHt ix)))

astFuncTy :: Ast.FuncTy -> Builder S.FuncType
astFuncTy ty = funcTy (Types.convertFuncTy ty)

funcTy :: Types.FuncTy -> Builder S.FuncType
funcTy = case _ of
  Types.FuncTy arguments result -> do
    arguments' <- traverse valTy arguments
    result' <- valTy result
    pure
      { arguments: arguments'
      , results: [ result' ]
      }

declareArrayType :: Types.Ty -> Builder S.TypeIdx
declareArrayType = case _ of
  Types.TyArray t -> do
    elTy <- valTy t
    Builder.declareType
      [ { final: true, supertypes: [], ty: S.CompArray { mutability: S.Var, ty: S.StorageVal elTy } } ]
  _ -> unsafeCrashWith "non-array type for array literal"

getFuncTask :: InitTask -> Maybe FillFunc
getFuncTask = case _ of
  FuncTask ft -> Just ft
  _ -> Nothing

getGlobalTask :: InitTask -> Maybe InitGlobal
getGlobalTask = case _ of
  GlobalTask it -> Just it
  _ -> Nothing

compileProgram :: CProgram -> S.Module
compileProgram toplevels = Builder.build' do
  fills <- traverse declareToplevel toplevels
  traverse_ implFunc (Array.mapMaybe (getFuncTask =<< _) fills)
  initializeGlobals (Array.mapMaybe (getGlobalTask =<< _) fills)

initializeGlobals :: Array InitGlobal -> Builder Unit
initializeGlobals igs = do
  fill <- Builder.declareFunc (FunctionV (-1)) { arguments: [], results: [] }
  fnBody <- bodyBuild [] do
    iss <- for igs \ig -> do
      is <- compileExpr ig.init
      pure (is <> [ S.GlobalSet ig.idx ])
    pure (Array.fold iss)
  fill fnBody.locals fnBody.result
  Builder.declareStart (FunctionV (-1))

declareToplevel :: CToplevel -> Builder (Maybe InitTask)
declareToplevel = case _ of
  Ast.TopLet name init -> do
    ty <- typeOf init
    case compileConst init of
      Just expr -> do
        _ <- Builder.declareGlobal name { mutability: S.Var, type: ty } expr
        pure Nothing
      Nothing -> do
        expr <- case Types.typeOf init of
          Types.TyI32 -> pure [ S.I32Const 0 ]
          Types.TyBool -> pure [ S.I32Const 0 ]
          Types.TyF32 -> pure [ S.F32Const 0.0 ]
          Types.TyUnit -> pure [ S.I32Const 0 ]
          t@(Types.TyArray _) -> do
            arrTy <- declareArrayType t
            pure [ S.RefNull (S.IndexHt arrTy) ]
        idx <- Builder.declareGlobal name { mutability: S.Var, type: ty } expr
        pure (Just (GlobalTask { idx, init }))
  Ast.TopFunc func -> do
    result <- declareFunc func
    case func.export of
      Nothing -> pure unit
      Just exportName -> Builder.declareExport func.name exportName
    pure (Just (FuncTask result))
  Ast.TopImport name ty externalName -> do
    tyIdx <- Builder.declareFuncType =<< astFuncTy ty
    _ <- Builder.declareImport name "env" externalName tyIdx
    pure Nothing

compileConst :: forall a b. Ast.Expr a b -> Maybe S.Expr
compileConst e = case e.expr of
  Ast.LitE (Ast.IntLit x) -> Just [ S.I32Const x ]
  Ast.LitE (Ast.FloatLit x) -> Just [ S.F32Const x ]
  _ -> Nothing

compileOp :: Types.Ty -> Ast.Op -> S.Instruction
compileOp = case _, _ of
  -- TODO: Potential optimization detect `== 0` and use `S.I32Eqz`
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
  Ast.VarE x -> map _.get (accessVar x)
  Ast.BinOpE op l r -> ado
    l' <- compileExpr l
    r' <- compileExpr r
    in l' <> r' <> [ compileOp (Types.typeOf l) op ]
  Ast.IfE cond t e -> ado
    cond' <- compileExpr cond
    t' <- compileExpr t
    e' <- compileExpr e
    ty <- Builder.liftBuilder (typeOf t)
    in cond' <> [ S.If (S.BlockValType (Just ty)) t' e' ]
  Ast.CallE fn args -> do
    args' <- traverse compileExpr args
    call <- Builder.liftBuilder case fn of
      BuiltinV bi -> pure bi.instr
      _ -> Builder.callImport fn >>= case _ of
        Just importCall -> pure importCall
        Nothing -> Builder.callFunc fn
    pure (Array.fold args' <> [ call ])
  Ast.IntrinsicE Ast.ArrayLen [ arr ] -> do
    arr' <- compileExpr arr
    pure (arr' <> [ S.ArrayLen ])
  Ast.IntrinsicE Ast.ArrayNew [ elem, size ] -> do
    ty <- Builder.liftBuilder (declareArrayType expr.note)
    elem' <- compileExpr elem
    size' <- compileExpr size
    pure (elem' <> size' <> [ S.ArrayNew ty ])
  Ast.IntrinsicE i args ->
    unsafeCrashWith ("invalid intrinsic call: " <> show i <> ", argcount " <> show (Array.length args))
  Ast.BlockE body -> compileBlock body
  Ast.ArrayE elements -> do
    tyIdx <- Builder.liftBuilder (declareArrayType expr.note)
    is <- traverse compileExpr elements
    pure (Array.fold is <> [ S.ArrayNewFixed tyIdx (Array.length elements) ])
  Ast.ArrayIdxE array idx -> do
    ty <- Builder.liftBuilder (declareArrayType array.note)
    arrayIs <- compileExpr array
    idxIs <- compileExpr idx
    pure (arrayIs <> idxIs <> [ S.ArrayGet ty ])

compileUnit :: S.Expr
compileUnit = [ S.I32Const 0 ]

compileBlock
  :: Array CDecl
  -> BodyBuilder S.Expr
compileBlock decls = do
  case Array.unsnoc decls of
    Just { init, last: Ast.ExprD expr } -> do
      instrs <- traverse go init
      result <- compileExpr expr
      pure (Array.concat instrs <> result)
    _ -> do
      instrs <- traverse go decls
      pure (Array.concat instrs <> compileUnit)
  where
  go :: CDecl -> BodyBuilder S.Expr
  go = case _ of
    Ast.ExprD expr -> do
      is <- compileExpr expr
      pure (is <> [ S.Drop ])
    Ast.LetD n e -> do
      ty <- Builder.liftBuilder (typeOf e)
      var <- Builder.newLocal n ty
      is <- compileExpr e
      pure (is <> [ S.LocalSet var ])
    Ast.SetD t e -> do
      case t of
        Ast.VarST n -> do
          var <- accessVar n
          is <- compileExpr e
          pure (is <> var.set)
        Ast.ArrayIdxST n ix -> do
          let elemType = Types.typeOf e
          tyArray <- Builder.liftBuilder (declareArrayType (Types.TyArray elemType))
          arrayVar <- accessVar n
          ixInstrs <- compileExpr ix
          eInstrs <- compileExpr e
          pure (arrayVar.get <> ixInstrs <> eInstrs <> [ S.ArraySet tyArray ])
    Ast.WhileD cond loopBody -> do
      cond' <- compileExpr cond
      loopBody' <- compileExpr loopBody
      let blockTy = S.BlockValType Nothing
      let neg bool = bool <> [ S.I32Eqz ]
      pure
        [ S.Block blockTy
            ( neg cond'
                <> [ S.Br_if 0 ]
                <>
                  [ S.Loop blockTy
                      ( loopBody'
                          <> neg cond'
                          <> [ S.Br_if 1, S.Br 0 ]
                      )
                  ]
            )
        ]

accessVar :: Var -> BodyBuilder { get :: S.Expr, set :: S.Expr }
accessVar v = case v of
  GlobalV _ -> do
    idx <- Builder.liftBuilder (Builder.lookupGlobal v)
    pure { get: [ S.GlobalGet idx ], set: [ S.GlobalSet idx ] }
  LocalV _ -> do
    idx <- Builder.lookupLocal v
    pure { get: [ S.LocalGet idx ], set: [ S.LocalSet idx ] }
  BuiltinV _ ->
    unsafeCrashWith "Can't reassign a built_in"
  FunctionV _ ->
    unsafeCrashWith "Can't reassign a function"

declareFunc :: CFunc -> Builder FillFunc
declareFunc func = do
  fTy <- astFuncTy (Ast.funcTyOf func)
  fill <- Builder.declareFunc func.name fTy
  pure { fill, func }

implFunc :: FillFunc -> Builder Unit
implFunc { fill, func } = do
  paramTys <- traverse (\v -> map (Tuple v.name) (astValTy v.ty)) func.params
  fnBody <- bodyBuild paramTys (compileExpr func.body)
  fill fnBody.locals fnBody.result
