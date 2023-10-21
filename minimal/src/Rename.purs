module Rename (renameProg) where

import Prelude

import Ast (Decl(..), Expr(..), Func(..), Program, Toplevel(..))
import Control.Monad.State (State)
import Control.Monad.State as State
import Data.List as List
import Data.List.NonEmpty as NEL
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe as Maybe
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)


-- | Takes a parsed Program and replaces all bound names with unique identifiers (Int's)
-- |
-- | Also returns a Map that maps every created identifier back to its original name
renameProg :: Program String -> { nameMap :: Map Int String, result :: Program Int }
renameProg prog = do
  let (Tuple prog' s) = State.runState (renameProg' prog) { scope: NEL.singleton Map.empty, nameMap: Map.empty, supply: 0 }
  { result: prog', nameMap: s.nameMap }

type Scope = NEL.NonEmptyList (Map String Int)

lookupScope :: String -> Scope -> Int
lookupScope n s = unsafePartial Maybe.fromJust (List.findMap (\blockScope -> Map.lookup n blockScope) s)

addVar :: String -> Int -> Scope -> Scope
addVar n idx scope = do
  let { head, tail } = NEL.uncons scope
  NEL.cons' (Map.insert n idx head) tail

type Rename a = State { scope :: Scope, supply :: Int, nameMap :: Map Int String } a

mkVar :: String -> Rename Int
mkVar name = do
  { supply } <- State.modify
    ( \s -> do
        let var = s.supply + 1
        s
          { supply = var
          , nameMap = Map.insert var name s.nameMap
          , scope = addVar name var s.scope
          }
    )
  pure supply

lookupVar :: String -> Rename Int
lookupVar name = do
  State.gets (\s -> lookupScope name s.scope)

withBlock :: forall a. Rename a -> Rename a
withBlock f = do
  oldScope <- State.gets _.scope
  State.modify_ (\s -> s { scope = NEL.cons Map.empty s.scope })
  res <- f
  State.modify_ (_ { scope = oldScope })
  pure res

renameProg' :: Program String -> Rename (Program Int)
renameProg' prog = traverse renameToplevel prog

renameToplevel :: Toplevel String -> Rename (Toplevel Int)
renameToplevel = case _ of
  TopImport name ty externalName -> do
    var <- mkVar name
    pure (TopImport var ty externalName)
  TopFunc (Func name params body) -> do
    nameVar <- mkVar name
    withBlock do
      paramVars <- traverse mkVar params
      body' <- renameExpr body
      pure (TopFunc (Func nameVar paramVars body'))
  TopLet name expr -> do
    expr' <- renameExpr expr
    var <- mkVar name
    pure (TopLet var expr')

renameExpr :: Expr String -> Rename (Expr Int)
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
  CallE f a -> do
    f' <- renameExpr f
    a' <- renameExpr a
    pure (CallE f' a')
  BlockE b -> withBlock do
    decls <- traverse renameDecl b
    pure (BlockE decls)

renameDecl
  :: Decl String
  -> Rename (Decl Int)
renameDecl = case _ of
  LetD binder expr -> do
    expr' <- renameExpr expr
    var <- mkVar binder
    pure (LetD var expr')
  SetD binder expr -> do
    var <- lookupVar binder
    expr' <- renameExpr expr
    pure (SetD var expr')
  ExprD expr -> do
    expr' <- renameExpr expr
    pure (ExprD expr')
