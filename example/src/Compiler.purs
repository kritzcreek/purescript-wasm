module Compiler (compileProgram) where

import Prelude

import Ast as Ast
import Data.Array as Array
import Data.ArrayBuffer.Types (ArrayView, Uint8)
import Data.Maybe (Maybe(..))
import Data.Traversable (for, traverse, traverse_)
import Data.Tuple (Tuple(..))
import DynamicBuffer as DBuffer
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafeCrashWith)
import Rename (Var(..))
import Types as Types
import Unsafe.Coerce (unsafeCoerce)
import Wasm.Syntax as S
import WasmBuilder (bodyBuild)
import WasmBuilder as Builder

type Builder = Builder.Builder Var
type BodyBuilder = Builder.BodyBuilder Var

type CFunc = Ast.Func (Ast.Ty Var) Var
type CExpr = Ast.Expr (Ast.Ty Var) Var
type CDecl = Ast.Decl (Ast.Ty Var) Var
type CToplevel = Ast.Toplevel (Ast.Ty Var) Var
type CProgram = Ast.Program (Ast.Ty Var) Var

i32 :: S.ValType
i32 = S.NumType S.I32

f32 :: S.ValType
f32 = S.NumType S.F32

typeOf :: CExpr -> Builder S.ValType
typeOf e = valTy (Types.typeOf e)

valTy :: Ast.Ty Var -> Builder S.ValType
valTy = case _ of
  Ast.TyI32 -> pure i32
  Ast.TyF32 -> pure f32
  Ast.TyBool -> pure i32
  Ast.TyUnit -> pure i32
  Ast.TyText -> do
    ix <- textTy
    pure (S.RefType (S.HeapTypeRef true (S.IndexHt ix)))
  t@(Ast.TyArray _) -> do
    ix <- declareArrayType t
    pure (S.RefType (S.HeapTypeRef true (S.IndexHt ix)))
  Ast.TyCons v -> do
    Tuple ix _ <- Builder.lookupStruct v
    pure (S.RefType (S.HeapTypeRef true (S.IndexHt ix)))

textTy :: Builder S.TypeIdx
textTy = Builder.declareType
  [ { final: true, supertypes: [], ty: S.CompArray { mutability: S.Var, ty: S.StoragePacked S.I8 } } ]

declareArrayType :: Ast.Ty Var -> Builder S.TypeIdx
declareArrayType = case _ of
  Ast.TyArray t -> do
    elTy <- valTy t
    Builder.declareType
      [ { final: true, supertypes: [], ty: S.CompArray { mutability: S.Var, ty: S.StorageVal elTy } } ]
  _ -> unsafeCrashWith "non-array type for array literal"

funcTy :: Ast.FuncTy Var -> Builder S.FuncType
funcTy = case _ of
  Ast.FuncTy arguments result -> do
    arguments' <- traverse valTy arguments
    result' <- valTy result
    pure
      { arguments: arguments'
      , results: [ result' ]
      }

compileProgram :: CProgram -> S.Module
compileProgram toplevels = Builder.build' do
  fills <- traverse declareToplevel toplevels
  traverse_ implFunc (Array.mapMaybe (getFuncTask =<< _) fills)
  initializeGlobals (Array.mapMaybe (getGlobalTask =<< _) fills)
  where
  implFunc { fill, func } = do
    paramTys <- traverse (\v -> map (Tuple v.name) (valTy v.ty)) func.params
    fnBody <- bodyBuild paramTys (compileExpr func.body)
    fill fnBody.locals fnBody.result

data InitTask = FuncTask FillFunc | GlobalTask InitGlobal

type FillFunc =
  { fill :: Array S.ValType -> S.Expr -> Builder Unit
  , func :: CFunc
  }

type InitGlobal =
  { idx :: S.GlobalIdx
  , init :: CExpr
  }

getFuncTask :: InitTask -> Maybe FillFunc
getFuncTask = case _ of
  FuncTask ft -> Just ft
  _ -> Nothing

getGlobalTask :: InitTask -> Maybe InitGlobal
getGlobalTask = case _ of
  GlobalTask it -> Just it
  _ -> Nothing

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
          Ast.TyI32 -> pure [ S.I32Const 0 ]
          Ast.TyBool -> pure [ S.I32Const 0 ]
          Ast.TyF32 -> pure [ S.F32Const 0.0 ]
          Ast.TyUnit -> pure [ S.I32Const 0 ]
          Ast.TyText -> do
            ix <- textTy
            pure [ S.RefNull (S.IndexHt ix) ]
          t@(Ast.TyArray _) -> do
            arrTy <- declareArrayType t
            pure [ S.RefNull (S.IndexHt arrTy) ]
          Ast.TyCons v -> do
            Tuple structTy _ <- Builder.lookupStruct v
            pure [ S.RefNull (S.IndexHt structTy) ]
        idx <- Builder.declareGlobal name { mutability: S.Var, type: ty } expr
        pure (Just (GlobalTask { idx, init }))
  Ast.TopFunc func -> do
    result <- declareFunc func
    case func.export of
      Nothing -> pure unit
      Just exportName -> Builder.declareExport func.name exportName
    pure (Just (FuncTask result))
  Ast.TopImport name ty externalName -> do
    tyIdx <- Builder.declareFuncType =<< funcTy ty
    _ <- Builder.declareImport name "env" externalName tyIdx
    pure Nothing
  Ast.TopStruct name fields -> do
    fields' <- for fields \field -> do
      fieldTy <- valTy field.ty
      pure { name: field.name, ty: { mutability: S.Var, ty: S.StorageVal fieldTy } }
    _ <- Builder.declareStructType name fields'
    pure Nothing

declareFunc :: CFunc -> Builder FillFunc
declareFunc func = do
  fTy <- funcTy (Ast.funcTyOf func)
  fill <- Builder.declareFunc func.name fTy
  pure { fill, func }

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

compileConst :: forall a b. Ast.Expr a b -> Maybe S.Expr
compileConst e = case e.expr of
  Ast.LitE (Ast.IntLit x) -> Just [ S.I32Const x ]
  Ast.LitE (Ast.FloatLit x) -> Just [ S.F32Const x ]
  _ -> Nothing

compileOp :: Ast.Ty Var -> Ast.Op -> S.Instruction
compileOp = case _, _ of
  -- TODO: Potential optimization detect `== 0` and use `S.I32Eqz`
  Ast.TyBool, Ast.Eq -> S.I32Eq
  Ast.TyBool, Ast.Neq -> S.I32Ne

  Ast.TyBool, Ast.And -> S.I32And
  Ast.TyBool, Ast.Or -> S.I32Or

  Ast.TyI32, Ast.Add -> S.I32Add
  Ast.TyI32, Ast.Sub -> S.I32Sub
  Ast.TyI32, Ast.Mul -> S.I32Mul
  Ast.TyI32, Ast.Div -> S.I32Div_s
  Ast.TyI32, Ast.Lt -> S.I32Lt_s
  Ast.TyI32, Ast.Gt -> S.I32Gt_s
  Ast.TyI32, Ast.Lte -> S.I32Le_s
  Ast.TyI32, Ast.Gte -> S.I32Ge_s
  Ast.TyI32, Ast.Eq -> S.I32Eq
  Ast.TyI32, Ast.Neq -> S.I32Ne

  Ast.TyF32, Ast.Add -> S.F32Add
  Ast.TyF32, Ast.Sub -> S.F32Sub
  Ast.TyF32, Ast.Mul -> S.F32Mul
  Ast.TyF32, Ast.Div -> S.F32Div
  Ast.TyF32, Ast.Lt -> S.F32Lt
  Ast.TyF32, Ast.Gt -> S.F32Gt
  Ast.TyF32, Ast.Lte -> S.F32Le
  Ast.TyF32, Ast.Gte -> S.F32Ge
  Ast.TyF32, Ast.Eq -> S.F32Eq
  Ast.TyF32, Ast.Neq -> S.F32Ne

  t, o ->
    unsafeCrashWith ("no instruction for operand: " <> show o <> " at type: " <> show t)

utf8Bytes :: String -> Array S.Byte
utf8Bytes s = unsafePerformEffect do
  bytes <- DBuffer.unsafeContents =<< DBuffer.fromUtf8 s
  -- TODO: Scary unsafeCoerce
  pure ((unsafeCoerce :: ArrayView Uint8 -> Array Int) bytes)

compileLit :: Ast.Lit -> Builder S.Expr
compileLit = case _ of
  Ast.IntLit x -> pure [ S.I32Const x ]
  Ast.FloatLit x -> pure [ S.F32Const x ]
  Ast.BoolLit b -> pure [ if b then S.I32Const 1 else S.I32Const 0 ]
  Ast.TextLit t -> do
    ty <- textTy
    ix <- Builder.declareData (utf8Bytes t) S.DataPassive
    pure [ S.ArrayInitData ty ix ]

compileExpr :: CExpr -> BodyBuilder S.Expr
compileExpr expr = case expr.expr of
  Ast.LitE lit -> Builder.liftBuilder (compileLit lit)
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
  Ast.StructE name fields -> do
    Tuple structTy fieldOrder <- Builder.liftBuilder (Builder.lookupStruct name)
    compiledFields <- for fields \f -> do
      fExpr <- compileExpr f.expr
      pure { name: f.name, instrs: fExpr }
    let
      is = map
        ( \fieldName ->
            case Array.find (\f -> f.name == fieldName) compiledFields of
              Nothing -> unsafeCrashWith ("missing field: " <> show fieldName)
              Just { instrs } -> instrs
        )
        fieldOrder
    pure (Array.fold is <> [ S.StructNew structTy ])
  Ast.StructIdxE structExpr index -> do
    expr' <- compileExpr structExpr
    (Tuple tyIdx fIdx) <- Builder.liftBuilder (Builder.lookupField index)
    pure (expr' <> [ S.StructGet tyIdx fIdx ])

compileUnit :: S.Expr
compileUnit = [ S.I32Const 0 ]

compileBlock
  :: Array CDecl
  -> BodyBuilder S.Expr
compileBlock decls = do
  case Array.unsnoc decls of
    Just { init, last: Ast.ExprD expr } -> do
      instrs <- traverse compileDecl init
      result <- compileExpr expr
      pure (Array.concat instrs <> result)
    _ -> do
      instrs <- traverse compileDecl decls
      pure (Array.concat instrs <> compileUnit)

compileDecl :: CDecl -> BodyBuilder S.Expr
compileDecl = case _ of
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
      Ast.VarST _ n -> do
        var <- accessVar n
        is <- compileExpr e
        pure (is <> var.set)
      Ast.ArrayIdxST ty n ix -> do
        tyArray <- Builder.liftBuilder (declareArrayType ty)
        arrayVar <- accessVar n
        ixInstrs <- compileExpr ix
        eInstrs <- compileExpr e
        pure (arrayVar.get <> ixInstrs <> eInstrs <> [ S.ArraySet tyArray ])
      Ast.StructIdxST _ n ix -> do
        Tuple structIdx fieldIdx <- Builder.liftBuilder (Builder.lookupField ix)
        structVar <- accessVar n
        eInstrs <- compileExpr e
        pure (structVar.get <> eInstrs <> [ S.StructSet structIdx fieldIdx ])
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
  TypeV _ ->
    unsafeCrashWith "Can't reassign a struct"
  FieldV _ ->
    unsafeCrashWith "Can't reassign a field"
