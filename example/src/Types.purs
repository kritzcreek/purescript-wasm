module Types where

import Prelude

import Ast as Ast
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either as Either
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(..))

data Ty
  = TyI32
  | TyF32
  | TyBool
  | TyUnit
  | TyArray Ty

showTy :: Ty -> String
showTy = case _ of
  TyI32 -> "i32"
  TyF32 -> "f32"
  TyBool -> "bool"
  TyUnit -> "unit"
  TyArray t -> "[" <> showTy t <> "]"

instance Show Ty where
  show = showTy

convertTy :: Ast.ValTy -> Ty
convertTy = case _ of
  Ast.TyI32 -> TyI32
  Ast.TyF32 -> TyF32
  Ast.TyBool -> TyBool
  Ast.TyUnit -> TyUnit
  Ast.TyArray t -> TyArray (convertTy t)

data FuncTy = FuncTy (Array Ty) Ty

convertFuncTy :: Ast.FuncTy -> FuncTy
convertFuncTy (Ast.FuncTy params result) = FuncTy (map convertTy params) (convertTy result)

type TypedExpr = Ast.Expr Ty String
type TypedSetTarget = Ast.SetTarget Ty String
type TypedDecl = Ast.Decl Ty String
type TypedToplevel = Ast.Toplevel Ty String
type TypedFunc = Ast.Func Ty String
type TypedProgram = Ast.Program Ty String

type Ctx = { funcs :: Map String FuncTy, vals :: Map String Ty }

addVal :: String -> Ty -> Ctx -> Ctx
addVal v t ctx = ctx { vals = Map.insert v t ctx.vals }

lookupVal :: String -> Ctx -> Either String Ty
lookupVal v ctx = Either.note ("Unknown variable: " <> v) (Map.lookup v ctx.vals)

typeOf :: forall name. Ast.Expr Ty name -> Ty
typeOf e = e.note

checkTy :: Ty -> Ty -> Either String Unit
checkTy = case _, _ of
  TyI32, TyI32 -> pure unit
  TyF32, TyF32 -> pure unit
  TyBool, TyBool -> pure unit
  TyUnit, TyUnit -> pure unit
  TyArray t, TyArray t' -> checkTy t t'
  expected, actual -> Left ("Expected " <> show expected <> ", but got " <> show actual)

checkTyNum :: Ty -> Either String Unit
checkTyNum = case _ of
  TyF32 -> pure unit
  TyI32 -> pure unit
  t -> Left ("Expected a numeric type but got: " <> show t)

checkTyArray :: Ty -> Either String Unit
checkTyArray = case _ of
  TyArray _ -> pure unit
  t -> Left ("Expected an array type but got: " <> show t)

checkNumericOperator :: Ty -> Ty -> Either String Ty
checkNumericOperator tyL tyR = do
  checkTyNum tyL
  checkTy tyL tyR
  pure tyL

checkComparisonOperator :: Ty -> Ty -> Either String Ty
checkComparisonOperator tyL tyR = do
  checkTyNum tyL
  checkTy tyL tyR
  pure TyBool

inferExpr :: forall note. Ctx -> Ast.Expr note String -> Either String TypedExpr
inferExpr ctx expr = case expr.expr of
  Ast.LitE lit ->
    pure { expr: Ast.LitE lit, note: inferLit lit }
  Ast.VarE v -> do
    t <- lookupVal v ctx
    pure { expr: Ast.VarE v, note: t }
  Ast.BinOpE o l r -> do
    l' <- inferExpr ctx l
    r' <- inferExpr ctx r
    let tyL = typeOf l'
    let tyR = typeOf r'
    ty <- case o of
      Ast.Eq -> pure TyBool
      Ast.Lt -> checkComparisonOperator tyL tyR
      Ast.Lte -> checkComparisonOperator tyL tyR
      Ast.Gt -> checkComparisonOperator tyL tyR
      Ast.Gte -> checkComparisonOperator tyL tyR
      Ast.Add -> checkNumericOperator tyL tyR
      Ast.Sub -> checkNumericOperator tyL tyR
      Ast.Mul -> checkNumericOperator tyL tyR
      Ast.Div -> checkNumericOperator tyL tyR
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
    FuncTy argTys resTy <- Either.note ("Unknown func: " <> fn) (Map.lookup fn ctx.funcs)
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

inferDecls :: forall note. Ctx -> Array (Ast.Decl note String) -> Either String { ty :: Ty, decls :: Array TypedDecl }
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
  -> Either String { ty :: Ty, target :: TypedSetTarget }
inferSetTarget ctx = case _ of
  Ast.VarST v -> do
    ty <- lookupVal v ctx
    pure { ty, target: Ast.VarST v }
  Ast.ArrayIdxST v ix -> do
    tyArray <- lookupVal v ctx
    case tyArray of
      TyArray elemTy -> do
        ix' <- inferExpr ctx ix
        checkTy TyI32 (typeOf ix')
        pure { ty: elemTy, target: Ast.ArrayIdxST v ix' }
      _ -> Left ("Tried to assign to a non-array target " <> show tyArray)

inferLit :: Ast.Lit -> Ty
inferLit = case _ of
  Ast.IntLit _ -> TyI32
  Ast.FloatLit _ -> TyF32
  Ast.BoolLit _ -> TyBool

topFuncTy :: forall note. Ast.Toplevel note String -> Maybe (Tuple String FuncTy)
topFuncTy = case _ of
  Ast.TopFunc f -> do
    let ty = FuncTy (map (convertTy <<< _.ty) f.params) (convertTy f.returnTy)
    Just (Tuple f.name ty)
  Ast.TopImport name ty _ ->
    Just (Tuple name (convertFuncTy ty))
  _ -> Nothing

inferToplevel :: forall note. Ctx -> Ast.Toplevel note String -> Either String { ctx :: Ctx, toplevel :: TypedToplevel }
inferToplevel ctx = case _ of
  Ast.TopLet v e -> do
    e' <- inferExpr ctx e
    pure { ctx: addVal v (typeOf e') ctx, toplevel: Ast.TopLet v e' }
  Ast.TopImport name ty externalName ->
    pure { ctx, toplevel: Ast.TopImport name ty externalName }
  Ast.TopFunc f -> do
    let ctx' = Array.foldl (\c param -> addVal param.name (convertTy param.ty) c) ctx f.params
    body <- inferExpr ctx' f.body
    checkTy (convertTy f.returnTy) (typeOf body)
    pure
      { ctx
      , toplevel: Ast.TopFunc (f { body = body })
      }

inferProgram :: forall note. Ast.Program note String -> Either String TypedProgram
inferProgram toplevels = do
  let funcTys = Array.mapMaybe topFuncTy toplevels
  let funcCtx = { funcs: Map.fromFoldable funcTys, vals: Map.empty }
  result <- Array.foldM
    ( \acc toplevel -> do
        { ctx: newCtx, toplevel: toplevel' } <- inferToplevel acc.ctx toplevel
        pure { ctx: newCtx, toplevels: Array.snoc acc.toplevels toplevel' }
    )
    { ctx: funcCtx, toplevels: [] }
    toplevels
  pure result.toplevels
