module Rename (Var(..), printVar, renameProgram, findFunc) where

import Prelude

import Ast (Decl(..), Expr(..), Program, Toplevel(..))
import Control.Monad.State (State)
import Control.Monad.State as State
import Data.Array as Array
import Data.List as List
import Data.List.NonEmpty as NEL
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

data Var
  = GlobalV Int
  | LocalV Int
  | FunctionV Int

derive instance Eq Var
derive instance Ord Var
instance Show Var where
  show = printVar

printVar :: Var -> String
printVar = case _ of
  GlobalV x -> "$g" <> show x
  LocalV x -> "$l" <> show x
  FunctionV x -> "$f" <> show x

-- | Takes a parsed Program and replaces all bound names with unique identifiers (Int's)
-- |
-- | Also returns a Map that maps every created identifier back to its original name
renameProgram :: Program String -> { nameMap :: Map Var String, result :: Program Var }
renameProgram prog = do
  let (Tuple prog' s) = State.runState (renameProgram' prog) { scope: NEL.singleton Map.empty, nameMap: Map.empty, supply: 0 }
  { result: prog', nameMap: s.nameMap }

findFunc :: Map Var String -> String -> Var
findFunc nameMap name = unsafePartial Maybe.fromJust (Array.findMap search (Map.toUnfoldable nameMap))
  where
  search = case _ of
    Tuple v@(FunctionV _) n | n == name -> Just v
    _ -> Nothing

type Scope = NEL.NonEmptyList (Map String Var)

lookupScope :: String -> Scope -> Var
lookupScope n s = unsafePartial Maybe.fromJust (List.findMap (\blockScope -> Map.lookup n blockScope) s)

addVar :: String -> Var -> Scope -> Scope
addVar n idx scope = do
  let { head, tail } = NEL.uncons scope
  NEL.cons' (Map.insert n idx head) tail

type Rename a = State { scope :: Scope, supply :: Int, nameMap :: Map Var String } a

mkVar :: (Int -> Var) -> String -> Rename Var
mkVar mk name = do
  { supply } <- State.modify
    ( \s -> do
        let var = mk (s.supply + 1)
        s
          { supply = s.supply + 1
          , nameMap = Map.insert var name s.nameMap
          , scope = addVar name var s.scope
          }
    )
  pure (mk supply)

lookupVar :: String -> Rename Var
lookupVar name = do
  State.gets (\s -> lookupScope name s.scope)

withBlock :: forall a. Rename a -> Rename a
withBlock f = do
  oldScope <- State.gets _.scope
  State.modify_ (\s -> s { scope = NEL.cons Map.empty s.scope })
  res <- f
  State.modify_ (_ { scope = oldScope })
  pure res

renameProgram' :: Program String -> Rename (Program Var)
renameProgram' prog = traverse renameToplevel prog

renameToplevel :: Toplevel String -> Rename (Toplevel Var)
renameToplevel = case _ of
  TopImport name ty externalName -> do
    var <- mkVar FunctionV name
    pure (TopImport var ty externalName)
  TopFunc { name, export, params, body } -> do
    nameVar <- mkVar FunctionV name
    withBlock do
      paramVars <- traverse (mkVar LocalV) params
      body' <- renameExpr body
      pure (TopFunc { name: nameVar, export, params: paramVars, body: body' })
  TopLet name expr -> do
    expr' <- renameExpr expr
    var <- mkVar GlobalV name
    pure (TopLet var expr')

renameExpr :: Expr String -> Rename (Expr Var)
renameExpr = case _ of
  LitE lit -> pure (LitE lit)
  VarE v -> do
    var <- lookupVar v
    pure (VarE var)
  BinOpE o l r -> do
    l' <- renameExpr l
    r' <- renameExpr r
    pure (BinOpE o l' r')
  IfE c t e -> do
    c' <- renameExpr c
    t' <- renameExpr t
    e' <- renameExpr e
    pure (IfE c' t' e')
  CallE fn args -> do
    fn' <- lookupVar fn
    args' <- traverse renameExpr args
    pure (CallE fn' args')
  BlockE b -> withBlock do
    decls <- traverse renameDecl b
    pure (BlockE decls)

renameDecl
  :: Decl String
  -> Rename (Decl Var)
renameDecl = case _ of
  LetD binder expr -> do
    expr' <- renameExpr expr
    var <- mkVar LocalV binder
    pure (LetD var expr')
  SetD binder expr -> do
    var <- lookupVar binder
    expr' <- renameExpr expr
    pure (SetD var expr')
  ExprD expr -> do
    expr' <- renameExpr expr
    pure (ExprD expr')
