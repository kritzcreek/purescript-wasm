module Types where

import Prelude

import Ast (FuncTy(..), Ty(..))
import Ast as Ast
import Builtins as Builtins
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either as Either
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (for_, traverse, traverse_)
import Data.Tuple (Tuple(..))

type TypedExpr = Ast.Expr (Ty String) String
type TypedSetTarget = Ast.SetTarget (Ty String) String
type TypedDecl = Ast.Decl (Ty String) String
type TypedToplevel = Ast.Toplevel (Ty String) String
type TypedFunc = Ast.Func (Ty String) String
type TypedProgram = Ast.Program (Ty String) String

type StructFields = Array { name :: String, ty :: Ty String }
type Ctx =
  { funcs :: Map String (FuncTy String)
  , vals :: Map String (Ty String)
  , structs :: Map String StructFields
  }

addVal :: String -> Ty String -> Ctx -> Ctx
addVal v t ctx = ctx { vals = Map.insert v t ctx.vals }

lookupVal :: Ctx -> String -> Either String (Ty String)
lookupVal ctx v = Either.note ("Unknown variable: " <> v) (Map.lookup v ctx.vals)

lookupStruct :: Ctx -> String -> Either String StructFields
lookupStruct ctx name = Either.note ("Unknown type: " <> name) (Map.lookup name ctx.structs)

typeOf :: forall n1 n2. Ast.Expr (Ty n1) n2 -> Ty n1
typeOf e = e.note

checkTy :: Ty String -> Ty String -> Either String Unit
checkTy = case _, _ of
  TyI32, TyI32 -> pure unit
  TyF32, TyF32 -> pure unit
  TyBool, TyBool -> pure unit
  TyUnit, TyUnit -> pure unit
  TyArray t, TyArray t' -> checkTy t t'
  TyCons t, TyCons t' | t == t' -> pure unit
  expected, actual -> Left ("Expected " <> show expected <> ", but got " <> show actual)

checkTyNum :: forall name. Show name => Ty name -> Either String Unit
checkTyNum = case _ of
  TyF32 -> pure unit
  TyI32 -> pure unit
  t -> Left ("Expected a numeric type but got: " <> show t)

checkTyArray :: forall name. Show name => Ty name -> Either String Unit
checkTyArray = case _ of
  TyArray _ -> pure unit
  t -> Left ("Expected an array type but got: " <> show t)

checkNumericOperator :: Ty String -> Ty String -> Either String (Ty String)
checkNumericOperator tyL tyR = do
  checkTyNum tyL
  checkTy tyL tyR
  pure tyL

checkBoolOperator :: Ty String -> Ty String -> Either String (Ty String)
checkBoolOperator tyL tyR = do
  checkTy TyBool tyL
  checkTy TyBool tyR
  pure TyBool

checkComparisonOperator :: Ty String -> Ty String -> Either String (Ty String)
checkComparisonOperator tyL tyR = do
  checkTyNum tyL
  checkTy tyL tyR
  pure TyBool

inferExpr :: forall note. Ctx -> Ast.Expr note String -> Either String TypedExpr
inferExpr ctx expr = case expr.expr of
  Ast.LitE lit ->
    pure { expr: Ast.LitE lit, note: inferLit lit }
  Ast.VarE v -> do
    t <- lookupVal ctx v
    pure { expr: Ast.VarE v, note: t }
  Ast.BinOpE o l r -> do
    l' <- inferExpr ctx l
    r' <- inferExpr ctx r
    let tyL = typeOf l'
    let tyR = typeOf r'
    ty <- case o of
      Ast.Eq -> pure TyBool
      Ast.Neq -> pure TyBool
      Ast.Lt -> checkComparisonOperator tyL tyR
      Ast.Lte -> checkComparisonOperator tyL tyR
      Ast.Gt -> checkComparisonOperator tyL tyR
      Ast.Gte -> checkComparisonOperator tyL tyR
      Ast.Add -> checkNumericOperator tyL tyR
      Ast.Sub -> checkNumericOperator tyL tyR
      Ast.Mul -> checkNumericOperator tyL tyR
      Ast.Div -> checkNumericOperator tyL tyR
      Ast.And -> checkBoolOperator tyL tyR
      Ast.Or -> checkBoolOperator tyL tyR
    pure { expr: Ast.BinOpE o l' r', note: ty }
  Ast.IfE c t e -> do
    c' <- inferExpr ctx c
    checkTy TyBool (typeOf c')
    t' <- inferExpr ctx t
    let ty = typeOf t'
    e' <- inferExpr ctx e
    checkTy ty (typeOf e')
    pure { expr: Ast.IfE c' t' e', note: ty }
  Ast.CallE fn args -> do
    let
      funcTy = case Builtins.find fn of
        Just { ty } -> Just ty
        Nothing -> Map.lookup fn ctx.funcs
    FuncTy argTys resTy <- Either.note ("Unknown func: " <> fn) funcTy
    if Array.length args /= Array.length argTys then
      Left
        ( "mismatched argument count for: " <> fn
            <> ", expected "
            <> show (Array.length argTys)
            <> " but got "
            <> show (Array.length args)
        )
    else
      pure unit
    args' <- traverse (inferExpr ctx) args
    _ <- Array.zipWithA (\expected arg -> checkTy expected (typeOf arg)) argTys args'
    pure { expr: Ast.CallE fn args', note: resTy }
  Ast.IntrinsicE i args -> do
    args' <- traverse (inferExpr ctx) args
    ty <- case i, args' of
      Ast.ArrayLen, [ arr ] -> do
        checkTyArray (typeOf arr)
        pure TyI32
      Ast.ArrayNew, [ elem, size ] -> do
        checkTy TyI32 (typeOf size)
        pure (TyArray (typeOf elem))
      _, _ -> do
        Left ("invalid arguments for intrinsic: " <> show i)
    pure { expr: Ast.IntrinsicE i args', note: ty }
  Ast.BlockE decls -> do
    result <- inferDecls ctx decls
    pure { expr: Ast.BlockE result.decls, note: result.ty }
  Ast.ArrayE elements -> do
    elements' <- traverse (inferExpr ctx) elements
    case Array.head elements' of
      -- TODO: empty array requires bidirectional checking
      Nothing -> pure { expr: Ast.ArrayE [], note: TyArray TyUnit }
      Just el -> do
        let tyEl = typeOf el
        traverse_ (checkTy tyEl <<< typeOf) elements'
        pure { expr: Ast.ArrayE elements', note: TyArray tyEl }
  Ast.ArrayIdxE arr idx -> do
    arr' <- inferExpr ctx arr
    idx' <- inferExpr ctx idx
    case typeOf arr' of
      TyArray tyEl -> do
        checkTy TyI32 (typeOf idx')
        pure { expr: Ast.ArrayIdxE arr' idx', note: tyEl }
      _ -> do
        Left ("Expected array type but got: " <> show (typeOf arr'))
  Ast.StructE ty fields -> do
    expectedFields <- lookupStruct ctx ty
    when (Array.length fields /= Array.length expectedFields) do
      Left ("Mismatched field count when constructing " <> ty)
    fields' <- traverse (\f -> f { expr = _ } <$> inferExpr ctx f.expr) fields
    -- TODO: Error on duplicated field names
    for_ fields' \f -> do
      ef <- Either.note ("Unknown field name: " <> f.name) (Array.find (\ef -> ef.name == f.name) expectedFields)
      checkTy ef.ty (typeOf f.expr)
    pure { expr: Ast.StructE ty fields', note: TyCons ty }
  Ast.StructIdxE struct idx -> do
    struct' <- inferExpr ctx struct
    case typeOf struct' of
      TyCons t -> do
        fields <- lookupStruct ctx t
        field <- Either.note
          ("Unknown field " <> idx <> " for type " <> show t)
          (Array.find (\f -> f.name == idx) fields)
        pure { expr: Ast.StructIdxE struct' idx, note: field.ty }
      t -> Left ("Can't project out of " <> show t)

inferDecls :: forall note. Ctx -> Array (Ast.Decl note String) -> Either String { ty :: Ty String, decls :: Array TypedDecl }
inferDecls initialCtx initialDecls = do
  result <- Array.foldM
    ( \({ ctx, decls }) -> case _ of
        Ast.LetD v expr -> do
          expr' <- inferExpr ctx expr
          let ty = typeOf expr'
          pure { ctx: addVal v ty ctx, decls: Array.snoc decls (Ast.LetD v expr') }
        Ast.SetD target expr -> do
          { ty: tyVar, target: target' } <- inferSetTarget ctx target
          expr' <- inferExpr ctx expr
          let ty = typeOf expr'
          checkTy tyVar ty
          pure { ctx, decls: Array.snoc decls (Ast.SetD target' expr') }
        Ast.ExprD expr -> do
          expr' <- inferExpr ctx expr
          pure { ctx, decls: Array.snoc decls (Ast.ExprD expr') }
        Ast.WhileD cond expr -> do
          cond' <- inferExpr ctx cond
          checkTy TyBool (typeOf cond')
          expr' <- inferExpr ctx expr
          checkTy TyUnit (typeOf expr')
          pure { ctx, decls: Array.snoc decls (Ast.WhileD cond' expr') }
    )
    { ctx: initialCtx, decls: [] }
    initialDecls
  let
    ty = case Array.last result.decls of
      Just (Ast.ExprD e) -> typeOf e
      _ -> TyUnit
  pure { ty, decls: result.decls }

inferSetTarget
  :: forall note
   . Ctx
  -> Ast.SetTarget note String
  -> Either String { ty :: Ty String, target :: TypedSetTarget }
inferSetTarget ctx = case _ of
  Ast.VarST _ v -> do
    ty <- lookupVal ctx v
    pure { ty, target: Ast.VarST ty v }
  Ast.ArrayIdxST _ v ix -> do
    tyArray <- lookupVal ctx v
    case tyArray of
      TyArray elemTy -> do
        ix' <- inferExpr ctx ix
        checkTy TyI32 (typeOf ix')
        pure { ty: elemTy, target: Ast.ArrayIdxST tyArray v ix' }
      _ -> Left ("Tried to assign to a non-array target " <> show tyArray)
  Ast.StructIdxST _ v ix -> do
    tyCons <- lookupVal ctx v
    case tyCons of
      TyCons tyStruct -> do
        sfs <- lookupStruct ctx tyStruct
        field <- Either.note "Tried to assign to non-existing field" (Array.find (\field -> field.name == ix) sfs)
        pure { ty: field.ty, target: Ast.StructIdxST tyCons v ix }
      _ -> Left ("Tried to assign to a non-struct target " <> show tyCons)

inferLit :: forall name. Ast.Lit -> Ty name
inferLit = case _ of
  Ast.IntLit _ -> TyI32
  Ast.FloatLit _ -> TyF32
  Ast.BoolLit _ -> TyBool

topFuncTy :: forall note. Ast.Toplevel note String -> Maybe (Tuple String (FuncTy String))
topFuncTy = case _ of
  Ast.TopFunc f -> do
    let ty = FuncTy (map _.ty f.params) f.returnTy
    Just (Tuple f.name ty)
  Ast.TopImport name ty _ ->
    Just (Tuple name ty)
  _ -> Nothing

inferToplevel :: forall note. Ctx -> Ast.Toplevel note String -> Either String { ctx :: Ctx, toplevel :: TypedToplevel }
inferToplevel ctx = case _ of
  Ast.TopLet v e -> do
    e' <- inferExpr ctx e
    pure { ctx: addVal v (typeOf e') ctx, toplevel: Ast.TopLet v e' }
  Ast.TopImport name ty externalName ->
    pure { ctx, toplevel: Ast.TopImport name ty externalName }
  Ast.TopFunc f -> do
    let ctx' = Array.foldl (\c param -> addVal param.name param.ty c) ctx f.params
    body <- inferExpr ctx' f.body
    checkTy f.returnTy (typeOf body)
    pure
      { ctx
      , toplevel: Ast.TopFunc (f { body = body })
      }
  Ast.TopStruct name fields -> do
    let ctx' = ctx { structs = Map.insert name fields ctx.structs }
    -- TODO: Check field types to refer to existing types
    pure
      { ctx: ctx'
      , toplevel: Ast.TopStruct name fields
      }

-- TODO Should forward declare all funcs and types
inferProgram :: forall note. Ast.Program note String -> Either String TypedProgram
inferProgram toplevels = do
  let funcTys = Array.mapMaybe topFuncTy toplevels
  let funcCtx = { funcs: Map.fromFoldable funcTys, vals: Map.empty, structs: Map.empty }
  result <- Array.foldM
    ( \acc toplevel -> do
        { ctx: newCtx, toplevel: toplevel' } <- inferToplevel acc.ctx toplevel
        pure { ctx: newCtx, toplevels: Array.snoc acc.toplevels toplevel' }
    )
    { ctx: funcCtx, toplevels: [] }
    toplevels
  pure result.toplevels
