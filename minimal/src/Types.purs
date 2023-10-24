module Types where

import Prelude

import Ast (Lit, Op)
import Ast as Ast
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either as Either
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

data Ty
  = TyI32
  | TyBool
  | TyUnit

instance Show Ty where
  show = case _ of
    TyI32 -> "i32"
    TyBool -> "bool"
    TyUnit -> "unit"

convertTy :: Ast.ValTy -> Ty
convertTy = case _ of
  Ast.TyI32 -> TyI32
  Ast.TyBool -> TyBool
  Ast.TyUnit -> TyUnit

data FuncTy = FuncTy (Array Ty) Ty

convertFuncTy :: Ast.FuncTy -> FuncTy
convertFuncTy (Ast.FuncTy params result) = FuncTy (map convertTy params) (convertTy result)

data TypedExpr
  = LitE Ty Lit
  | VarE Ty String
  | BinOpE Ty Op TypedExpr TypedExpr
  | IfE Ty TypedExpr TypedExpr TypedExpr
  | CallE Ty String FuncTy (Array TypedExpr)
  | BlockE Ty (Array TypedDecl)

data TypedDecl
  = LetD Ty String TypedExpr
  | SetD Ty String TypedExpr
  | ExprD TypedExpr

data TypedToplevel
  = TopFunc Func
  | TopLet Ty String TypedExpr
  | TopImport String FuncTy String

type Func =
  { name :: String
  , export :: Maybe String
  , params :: Array { name :: String, ty :: Ty }
  , returnTy :: Ty
  , body :: TypedExpr
  }

type TypedProgram = Array TypedToplevel

type Ctx = { funcs :: Map String FuncTy, vals :: Map String Ty }

addVal :: String -> Ty -> Ctx -> Ctx
addVal v t ctx = ctx { vals = Map.insert v t ctx.vals }

lookupVal :: String -> Ctx -> Either String Ty
lookupVal v ctx = Either.note ("Unknown variable: " <> v) (Map.lookup v ctx.vals)

typeOf :: TypedExpr -> Ty
typeOf = case _ of
  LitE t _ -> t
  VarE t _ -> t
  BinOpE t _ _ _ -> t
  IfE t _ _ _ -> t
  CallE t _ _ _ -> t
  BlockE t _ -> t

checkTy :: Ty -> Ty -> Either String Unit
checkTy = case _, _ of
  TyI32, TyI32 -> pure unit
  TyBool, TyBool -> pure unit
  TyUnit, TyUnit -> pure unit
  expected, actual -> Left ("Expected " <> show expected <> ", but got " <> show actual)

inferExpr :: Ctx -> Ast.Expr String -> Either String TypedExpr
inferExpr ctx = case _ of
  Ast.LitE lit -> pure (LitE (inferLit lit) lit)
  Ast.VarE v -> do
    t <- lookupVal v ctx
    pure (VarE t v)
  Ast.BinOpE o l r -> do
    l' <- inferExpr ctx l
    r' <- inferExpr ctx r
    ty <- case o of
      Ast.Eq -> do
        pure TyBool
      _ -> do
        checkTy TyI32 (typeOf l')
        checkTy TyI32 (typeOf r')
        pure TyI32
    pure (BinOpE ty o l' r')
  Ast.IfE c t e -> do
    c' <- inferExpr ctx c
    checkTy TyBool (typeOf c')
    t' <- inferExpr ctx t
    let ty = typeOf t'
    e' <- inferExpr ctx e
    checkTy ty (typeOf e')
    pure (IfE ty c' t' e')
  Ast.CallE fn args -> do
    tyFunc@(FuncTy argTys resTy) <- Either.note ("Unknown func: " <> fn) (Map.lookup fn ctx.funcs)
    if Array.length args /= Array.length argTys then
      Left
        ( "mismatched argument count for :" <> fn
            <> ", expected "
            <> show (Array.length argTys)
            <> " but got "
            <> show (Array.length args)
        )
    else
      pure unit
    args' <- traverse (inferExpr ctx) args
    _ <- Array.zipWithA (\expected arg -> checkTy expected (typeOf arg)) argTys args'
    pure (CallE resTy fn tyFunc args')
  Ast.BlockE decls -> do
    result <- inferDecls ctx decls
    pure (BlockE result.ty result.decls)

inferDecls :: Ctx -> Array (Ast.Decl String) -> Either String { ty :: Ty, decls :: Array TypedDecl }
inferDecls initialCtx initialDecls = do
  result <- Array.foldM
    ( \({ ctx, decls }) -> case _ of
        Ast.LetD v expr -> do
          expr' <- inferExpr ctx expr
          let ty = typeOf expr'
          pure { ctx: addVal v ty ctx, decls: Array.snoc decls (LetD ty v expr') }
        Ast.SetD v expr -> do
          tyVar <- lookupVal v ctx
          expr' <- inferExpr ctx expr
          let ty = typeOf expr'
          checkTy tyVar ty
          pure { ctx, decls: Array.snoc decls (SetD ty v expr') }
        Ast.ExprD expr -> do
          expr' <- inferExpr ctx expr
          pure { ctx, decls: Array.snoc decls (ExprD expr') }
    )
    { ctx: initialCtx, decls: [] }
    initialDecls
  let
    ty = case Array.last (result.decls) of
      Just (ExprD e) -> typeOf e
      _ -> TyUnit
  pure { ty, decls: result.decls }

inferLit :: Ast.Lit -> Ty
inferLit = case _ of
  Ast.IntLit _ -> TyI32
  Ast.BoolLit _ -> TyBool

topFuncTy :: Ast.Toplevel String -> Maybe (Tuple String FuncTy)
topFuncTy = case _ of
  Ast.TopFunc f -> do
    let ty = FuncTy (map (convertTy <<< _.ty) f.params) (convertTy f.returnTy)
    Just (Tuple f.name ty)
  Ast.TopImport name ty _ ->
    Just (Tuple name (convertFuncTy ty))
  _ -> Nothing

inferToplevel :: Ctx -> Ast.Toplevel String -> Either String { ctx :: Ctx, toplevel :: TypedToplevel }
inferToplevel ctx = case _ of
  Ast.TopLet v e -> do
    e' <- inferExpr ctx e
    pure { ctx: addVal v (typeOf e') ctx, toplevel: TopLet (typeOf e') v e' }
  Ast.TopImport name ty externalName ->
    pure { ctx, toplevel: TopImport name (convertFuncTy ty) externalName }
  Ast.TopFunc f -> do
    let ctx' = Array.foldl (\c param -> addVal param.name (convertTy param.ty) c) ctx f.params
    body <- inferExpr ctx' f.body
    pure
      { ctx
      , toplevel: TopFunc
          { name: f.name
          , export: f.export
          , params: map (\p -> { name: p.name, ty: convertTy p.ty }) f.params
          , body
          , returnTy: convertTy f.returnTy
          }
      }

inferProgram :: Ast.Program String -> Either String TypedProgram
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
