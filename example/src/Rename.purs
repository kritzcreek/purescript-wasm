module Rename (Var(..), printVar, renameProgram) where

import Prelude

import Ast (Decl(..), Expr, Expr'(..), FuncTy(..), Program, SetTarget(..), Toplevel(..), Ty(..))
import Builtins as Builtins
import Control.Monad.State (State)
import Control.Monad.State as State
import Data.List as List
import Data.List.NonEmpty as NEL
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Traversable (for, for_, traverse)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafeCrashWith, unsafePartial)

data Var
  = GlobalV Int
  | LocalV Int
  | FunctionV Int
  | TypeV Int
  | FieldV Int
  | BuiltinV Builtins.Fn

derive instance Eq Var
derive instance Ord Var
instance Show Var where
  show = printVar

printVar :: Var -> String
printVar = case _ of
  GlobalV x -> "$g" <> show x
  LocalV x -> "$l" <> show x
  FunctionV x -> "$fn" <> show x
  TypeV x -> "$t" <> show x
  FieldV x -> "$f" <> show x
  BuiltinV n -> n.name

-- | Takes a parsed Program and replaces all bound names with unique identifiers (Int's)
-- |
-- | Also returns a Map that maps every created identifier back to its original name
renameProgram :: Program (Ty String) String -> { nameMap :: Map Var String, result :: Program (Ty Var) Var }
renameProgram prog = do
  let (Tuple prog' s) = State.runState (renameProgram' prog) { scope: NEL.singleton Map.empty, nameMap: Map.empty, supply: 0, types: Map.empty }
  { result: prog', nameMap: s.nameMap }

type Scope = NEL.NonEmptyList (Map String Var)

lookupScope :: String -> Scope -> Var
lookupScope n s = unsafePartial Maybe.fromJust (List.findMap (\blockScope -> Map.lookup n blockScope) s)

addVar :: String -> Var -> Scope -> Scope
addVar n idx scope = do
  let { head, tail } = NEL.uncons scope
  NEL.cons' (Map.insert n idx head) tail

addType :: String -> Array String -> Rename Unit
addType name fields = do
  nameVar <- mkVar TypeV name
  fieldVars <- traverse (\name' -> Tuple name' <$> mkVar FieldV name') fields
  State.modify_ (\s -> s { types = Map.insert name { var: nameVar, fields: Map.fromFoldable fieldVars } s.types })

type TypeInfo =
  { var :: Var
  , fields :: Map String Var
  }

type Rename a = State
  { scope :: Scope
  , types :: Map String TypeInfo
  , supply :: Int
  , nameMap :: Map Var String
  }
  a

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

lookupFunc :: String -> Rename Var
lookupFunc name =
  case Builtins.find name of
    Just bi -> pure (BuiltinV bi)
    Nothing -> lookupVar name

lookupTy :: String -> Rename TypeInfo
lookupTy name = do
  State.gets (\s -> unsafePartial Maybe.fromJust (Map.lookup name s.types))

lookupTyV :: Var -> Rename TypeInfo
lookupTyV v = do
  tys <- State.gets _.types
  pure (unsafePartial Maybe.fromJust (List.find (\ti -> ti.var == v) (Map.values tys)))

lookupField :: String -> TypeInfo -> Var
lookupField name info = unsafePartial Maybe.fromJust (Map.lookup name info.fields)

withBlock :: forall a. Rename a -> Rename a
withBlock f = do
  oldScope <- State.gets _.scope
  State.modify_ (\s -> s { scope = NEL.cons Map.empty s.scope })
  res <- f
  State.modify_ (_ { scope = oldScope })
  pure res

-- TODO: Forward declare function names and types
renameProgram' :: Program (Ty String) String -> Rename (Program (Ty Var) Var)
renameProgram' prog = do
  for_ prog case _ of
    TopStruct name fields -> addType name (map _.name fields)
    _ -> pure unit
  traverse renameToplevel prog

renameToplevel :: Toplevel (Ty String) String -> Rename (Toplevel (Ty Var) Var)
renameToplevel = case _ of
  TopImport name ty externalName -> do
    var <- mkVar FunctionV name
    ty' <- renameFuncTy ty
    pure (TopImport var ty' externalName)
  TopFunc { name, export, params, returnTy, body } -> do
    nameVar <- mkVar FunctionV name
    withBlock do
      paramVars <- traverse (\p -> { name: _, ty: _ } <$> mkVar LocalV p.name <*> renameTy p.ty) params
      body' <- renameExpr body
      returnTy' <- renameTy returnTy
      pure (TopFunc { name: nameVar, export, params: paramVars, returnTy: returnTy', body: body' })
  TopLet name expr -> do
    expr' <- renameExpr expr
    var <- mkVar GlobalV name
    pure (TopLet var expr')
  TopStruct name fields -> do
    tyInfo <- lookupTy name
    fields' <- for fields \f -> do
      let name' = lookupField f.name tyInfo
      ty' <- renameTy f.ty
      pure { name: name', ty: ty' }
    pure (TopStruct tyInfo.var fields')

renameExpr :: Expr (Ty String) String -> Rename (Expr (Ty Var) Var)
renameExpr expr = { note: _, expr: _ } <$> renameTy expr.note <*> case expr.expr of
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
    fn' <- lookupFunc fn
    args' <- traverse renameExpr args
    pure (CallE fn' args')
  IntrinsicE i args -> do
    args' <- traverse renameExpr args
    pure (IntrinsicE i args')
  BlockE b -> withBlock do
    decls <- traverse renameDecl b
    pure (BlockE decls)
  ArrayE els -> do
    els' <- traverse renameExpr els
    pure (ArrayE els')
  ArrayIdxE arr idx -> do
    arr' <- renameExpr arr
    idx' <- renameExpr idx
    pure (ArrayIdxE arr' idx')
  StructE ty fields -> do
    tyInfo <- lookupTy ty
    fields' <- for fields \field -> do
      let name' = lookupField field.name tyInfo
      expr' <- renameExpr field.expr
      pure { name: name', expr: expr' }
    pure (StructE tyInfo.var fields')
  StructIdxE struct idx -> do
    struct' <- renameExpr struct
    case struct'.note of
      TyCons tyV -> do
        tyInfo <- lookupTyV tyV
        let idx' = lookupField idx tyInfo
        pure (StructIdxE struct' idx')
      _ -> unsafeCrashWith "Tried to rename field access to a non-struct"

renameDecl :: Decl (Ty String) String -> Rename (Decl (Ty Var) Var)
renameDecl = case _ of
  LetD binder expr -> do
    expr' <- renameExpr expr
    var <- mkVar LocalV binder
    pure (LetD var expr')
  SetD target expr -> do
    target' <- renameSetTarget target
    expr' <- renameExpr expr
    pure (SetD target' expr')
  ExprD expr -> do
    expr' <- renameExpr expr
    pure (ExprD expr')
  WhileD cond expr -> do
    cond' <- renameExpr cond
    expr' <- renameExpr expr
    pure (WhileD cond' expr')

renameSetTarget
  :: SetTarget (Ty String) String
  -> Rename (SetTarget (Ty Var) Var)
renameSetTarget = case _ of
  VarST t binder -> do
    t' <- renameTy t
    var <- lookupVar binder
    pure (VarST t' var)
  ArrayIdxST t binder ix -> do
    t' <- renameTy t
    var <- lookupVar binder
    ix' <- renameExpr ix
    pure (ArrayIdxST t' var ix')
  StructIdxST t binder ix -> do
    ty <- case t of
      TyCons t' -> lookupTy t'
      _ -> unsafeCrashWith "tried to rename non-struct assignment"
    t' <- renameTy t
    binder' <- lookupVar binder
    let field = lookupField ix ty
    pure (StructIdxST t' binder' field)

renameFuncTy :: FuncTy String -> Rename (FuncTy Var)
renameFuncTy (FuncTy args result) = do
  args' <- traverse renameTy args
  result' <- renameTy result
  pure (FuncTy args' result')

renameTy :: Ty String -> Rename (Ty Var)
renameTy = case _ of
  TyI32 -> pure TyI32
  TyF32 -> pure TyF32
  TyBool -> pure TyBool
  TyUnit -> pure TyUnit
  TyText -> pure TyText
  TyArray t -> map TyArray (renameTy t)
  TyCons n -> do
    tyInfo <- lookupTy n
    pure (TyCons tyInfo.var)
