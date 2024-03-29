module WasmBuilder
  ( BodyBuilder
  , BuildError(..)
  , Builder
  , bodyBuild
  , build
  , build'
  , declareType
  , declareFuncType
  , declareStructType
  , declareExport
  , declareFunc
  , declareGlobal
  , declareImport
  , declareStart
  , lookupGlobal
  , lookupStruct
  , lookupField
  , lookupFunc
  , callFunc
  , callImport
  , newLocal
  , lookupLocal
  , liftBuilder
  ) where

import Prelude

import Control.Monad.Reader (ReaderT(..), runReaderT)
import Data.Array as Array
import Data.Either (Either(..))
import Data.FoldableWithIndex (forWithIndex_)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafeCrashWith)
import Record as Record
import Type.Proxy (Proxy(..))
import Wasm.Syntax (CompositeType(..), Export, ExportDesc(..), Expr, FieldIdx, FieldType, Func, FuncIdx, FuncType, Global, GlobalIdx, GlobalType, Import, ImportDesc(..), Instruction(..), LocalIdx, Memory, Module, Name, RecType, TypeIdx, ValType, emptyModule)

type FuncData =
  { index :: FuncIdx
  , type :: TypeIdx
  , locals :: Maybe (Array ValType)
  , body :: Maybe Expr
  }

type GlobalData =
  { index :: GlobalIdx
  , type :: GlobalType
  , init :: Expr
  }

type ImportData = { index :: FuncIdx, tyIdx :: TypeIdx, ns :: String, func :: String }

type Env name =
  { funcs :: Ref (Map name FuncData)
  , funcsSupply :: Ref Int
  , globals :: Ref (Map name GlobalData)
  , globalsSupply :: Ref Int
  , types :: Ref (Array RecType)
  , structs :: Ref (Map name (Tuple TypeIdx (Array name)))
  , fields :: Ref (Map name (Tuple TypeIdx FieldIdx))
  , memory :: Ref (Maybe Memory)
  , imports :: Ref (Map name ImportData)
  , exports :: Ref (Array Export)
  , startFn :: Ref (Maybe FuncIdx)
  }

initialEnv :: forall name. Effect (Env name)
initialEnv = ado
  imports <- Ref.new Map.empty
  funcs <- Ref.new Map.empty
  funcsSupply <- Ref.new (-1)
  globals <- Ref.new Map.empty
  globalsSupply <- Ref.new (-1)
  types <- Ref.new []
  structs <- Ref.new Map.empty
  fields <- Ref.new Map.empty
  memory <- Ref.new Nothing
  exports <- Ref.new []
  startFn <- Ref.new Nothing
  in { funcs, funcsSupply, globals, globalsSupply, types, structs, fields, memory, imports, exports, startFn }

newtype Builder name a = Builder (ReaderT (Env name) Effect a)

derive newtype instance Functor (Builder name)
derive newtype instance Apply (Builder name)
derive newtype instance Applicative (Builder name)
derive newtype instance Bind (Builder name)
derive newtype instance Monad (Builder name)
derive newtype instance MonadEffect (Builder name) -- Remove as this is unsafe

mkBuilder :: forall name a. (Env name -> Effect a) -> Builder name a
mkBuilder act = Builder (ReaderT act)

declareType :: forall name. RecType -> Builder name TypeIdx
declareType ty = mkBuilder \{ types } -> do
  ts <- Ref.read types
  case Array.findIndex (_ == ty) ts of
    Nothing -> do
      Ref.write (Array.snoc ts ty) types
      pure (Array.length ts)
    Just ix ->
      pure ix

declareFuncType :: forall name. FuncType -> Builder name TypeIdx
declareFuncType ty = do
  let funcTy = [ { final: true, supertypes: [], ty: CompFunc ty } ]
  declareType funcTy

-- Error on double declaring struct?
declareStructType :: forall name. Ord name => name -> Array { name :: name, ty :: FieldType } -> Builder name TypeIdx
declareStructType name fieldTys = do
  tyIx <- declareType [ { final: true, supertypes: [], ty: CompStruct (map _.ty fieldTys) } ]
  mkBuilder \{ structs, fields } -> do
    Ref.modify_ (Map.insert name (Tuple tyIx (map _.name fieldTys))) structs
    forWithIndex_ fieldTys \i fTy ->
      Ref.modify_ (Map.insert fTy.name (Tuple tyIx i)) fields
  pure tyIx

lookupStruct :: forall name. Show name => Ord name => name -> Builder name (Tuple TypeIdx (Array name))
lookupStruct name = mkBuilder \{ structs } -> do
  map (Map.lookup name) (Ref.read structs) >>= case _ of
    Nothing -> throw ("unknown struct " <> show name)
    Just idx -> pure idx

lookupField :: forall name. Show name => Ord name => name -> Builder name (Tuple TypeIdx FieldIdx)
lookupField name = mkBuilder \{ fields } -> do
  map (Map.lookup name) (Ref.read fields) >>= case _ of
    Nothing -> throw ("unknown field " <> show name)
    Just idxs -> pure idxs

nextGlobalIdx :: forall name. Builder name GlobalIdx
nextGlobalIdx =
  mkBuilder \{ globalsSupply } -> Ref.modify (_ + 1) globalsSupply

declareImport
  :: forall name
   . Ord name
  => name
  -> String
  -> String
  -> TypeIdx
  -> Builder name Unit
declareImport name ns func tyIdx = do
  index <- nextFuncIdx
  mkBuilder \{ imports } -> do
    Ref.modify_ (Map.insert name { index, tyIdx, ns, func }) imports

lookupFuncImport :: forall name. Show name => Ord name => name -> Builder name (Maybe FuncIdx)
lookupFuncImport name =
  mkBuilder \{ imports } -> do
    fs <- Ref.read imports
    pure (map _.index (Map.lookup name fs))

callImport :: forall name. Show name => Ord name => name -> Builder name (Maybe Instruction)
callImport name =
  map (map Call) (lookupFuncImport name)

declareGlobal
  :: forall name
   . Show name
  => Ord name
  => name
  -> GlobalType
  -> Expr
  -> Builder name GlobalIdx
declareGlobal name ty init = do
  index <- nextGlobalIdx
  mkBuilder \{ globals } -> do
    gs <- Ref.read globals
    when (Map.member name gs) do
      throw ("double declaring global: " <> show name)
    Ref.modify_ (Map.insert name { index, type: ty, init }) globals
    pure index

lookupGlobal
  :: forall name
   . Show name
  => Ord name
  => name
  -> Builder name GlobalIdx
lookupGlobal name = do
  mkBuilder \{ globals } -> do
    gs <- Ref.read globals
    case Map.lookup name gs of
      Nothing -> throw ("undeclared global: " <> show name)
      Just { index } -> pure index

nextFuncIdx :: forall name. Builder name FuncIdx
nextFuncIdx =
  mkBuilder \{ funcsSupply } -> Ref.modify (_ + 1) funcsSupply

declareFunc
  :: forall name
   . Show name
  => Ord name
  => name
  -> FuncType
  -> Builder name (Array ValType -> Expr -> Builder name Unit)
declareFunc name ty = do
  tyIndex <- declareFuncType ty
  index <- nextFuncIdx
  mkBuilder \{ funcs } -> do
    fs <- Ref.read funcs
    when (Map.member name fs) do
      throw ("double declaring function: " <> show name)
    Ref.write
      (Map.insert name { index, type: tyIndex, locals: Nothing, body: Nothing } fs)
      funcs
  pure \locals body ->
    mkBuilder \{ funcs } ->
      Ref.modify_ (Map.update (Just <<< _ { body = Just body, locals = Just locals }) name) funcs

lookupFunc :: forall name. Show name => Ord name => name -> Builder name FuncIdx
lookupFunc name =
  mkBuilder \{ funcs } -> do
    fs <- Ref.read funcs
    case Map.lookup name fs of
      Nothing -> throw ("undeclared function call: " <> show name)
      Just { index } -> pure index

callFunc :: forall name. Show name => Ord name => name -> Builder name Instruction
callFunc name =
  map Call (lookupFunc name)

declareExport :: forall name. Show name => Ord name => name -> Name -> Builder name Unit
declareExport name exportName = do
  index <- lookupFunc name
  mkBuilder \{ exports } ->
    Ref.modify_
      (\es -> Array.snoc es { name: exportName, desc: ExportFunc index })
      exports

declareStart :: forall name. Show name => Ord name => name -> Builder name Unit
declareStart name = do
  index <- lookupFunc name
  mkBuilder \({ startFn }) ->
    -- TODO: Error on double declaring start
    Ref.write (Just index) startFn

data BuildError name = MissingBody name

instance showBuildError :: Show name => Show (BuildError name) where
  show = case _ of
    MissingBody name -> "Missing body for: " <> show name

buildFuncs
  :: forall name
   . Show name
  => Env name
  -> Effect (Either (BuildError name) (Array Func))
buildFuncs { funcs } = do
  fs <- map Map.toUnfoldable (Ref.read funcs)
  let sortedFuncs = Array.sortWith (_.index <<< Tuple.snd) fs
  pure (traverse toWasmFunc sortedFuncs)
  where
  toWasmFunc :: Tuple name FuncData -> Either (BuildError name) Func
  toWasmFunc (Tuple name { type: ty, locals, body }) = case body, locals of
    Just b, Just l ->
      Right { type: ty, locals: l, body: b }
    _, _ ->
      Left (MissingBody name)

buildGlobals
  :: forall name
   . Show name
  => Env name
  -> Effect (Array Global)
buildGlobals { globals } = do
  gs <- map Map.toUnfoldable (Ref.read globals)
  let sortedGlobals = Array.sortWith (_.index <<< Tuple.snd) gs
  pure (map (Record.delete (Proxy :: _ "index") <<< Tuple.snd) sortedGlobals)

buildImports
  :: forall name
   . Env name
  -> Effect (Array Import)
buildImports { imports } = do
  is <- map Map.toUnfoldable (Ref.read imports)
  let sortedFuncImports = Array.sortWith (_.index <<< Tuple.snd) is
  pure (map (\(Tuple _ importData) -> { module: importData.ns, name: importData.func, desc: ImportFunc importData.tyIdx }) sortedFuncImports)

buildModule
  :: forall name
   . Show name
  => Env name
  -> Effect (Either (BuildError name) Module)
buildModule env@{ types, memory, exports } = do
  ts <- Ref.read types
  wasmGlobals <- buildGlobals env
  imps <- buildImports env
  mem <- Ref.read memory
  exps <- Ref.read exports
  startFn <- Ref.read env.startFn
  buildFuncs env <#> case _ of
    Left err ->
      Left err
    Right wasmFuncs ->
      Right
        ( emptyModule
            { funcs = wasmFuncs
            , globals = wasmGlobals
            , types = ts
            , memories = Array.fromFoldable mem
            , exports = exps
            , imports = imps
            , start = startFn
            }
        )

build :: forall name a. Show name => Builder name a -> Either (BuildError name) Module
build (Builder b) = unsafePerformEffect do
  env <- initialEnv
  _ <- runReaderT b env
  buildModule env

build' :: forall name a. Show name => Builder name a -> Module
build' b = case build b of
  Right m -> m
  Left err -> unsafeCrashWith (show err)

type LocalData = { index :: LocalIdx, ty :: ValType }

type BodyEnv name = { locals :: Ref (Map name LocalData) }

newtype BodyBuilder name a = BodyBuilder (ReaderT (BodyEnv name) (Builder name) a)

derive newtype instance Functor (BodyBuilder name)
derive newtype instance Apply (BodyBuilder name)
derive newtype instance Applicative (BodyBuilder name)
derive newtype instance Bind (BodyBuilder name)
derive newtype instance Monad (BodyBuilder name)

initialBodyEnv :: forall name. Ord name => Array (Tuple name ValType) -> Effect (BodyEnv name)
initialBodyEnv params = ado
  locals <- Ref.new (Map.fromFoldable (Array.mapWithIndex (\index (Tuple name ty) -> Tuple name { index, ty }) params))
  in { locals }

nextLocalIdx :: forall name. BodyBuilder name LocalIdx
nextLocalIdx =
  mkBodyBuilder \{ locals } -> liftEffect (map Map.size (Ref.read locals))

mkBodyBuilder :: forall name a. (BodyEnv name -> Builder name a) -> BodyBuilder name a
mkBodyBuilder act = BodyBuilder (ReaderT act)

bodyBuild
  :: forall name a
   . Ord name
  => Array (Tuple name ValType)
  -> BodyBuilder name a
  -> Builder name { result :: a, locals :: Array ValType }
bodyBuild params (BodyBuilder b) = do
  env <- liftEffect (initialBodyEnv params)
  result <- runReaderT b env
  ls <- liftEffect (Ref.read env.locals)
  let
    paramCount = Array.length params
    locals =
      Map.values ls
        -- remove all params
        # List.filter (\local -> local.index >= paramCount)
        # Array.fromFoldable
        # Array.sortWith _.index
        # map _.ty
  pure { result, locals }

newLocal
  :: forall name
   . Ord name
  => name
  -> ValType
  -> BodyBuilder name LocalIdx
newLocal name ty = do
  index <- nextLocalIdx
  mkBodyBuilder \{ locals } -> liftEffect do
    Ref.modify_ (Map.insert name { index, ty }) locals
    pure index

lookupLocal
  :: forall name
   . Show name
  => Ord name
  => name
  -> BodyBuilder name LocalIdx
lookupLocal name = do
  mkBodyBuilder \{ locals } -> liftEffect do
    ls <- Ref.read locals
    case Map.lookup name ls of
      Nothing -> throw ("undeclared local: " <> show name)
      Just { index } -> pure index

liftBuilder :: forall name a. Builder name a -> BodyBuilder name a
liftBuilder b = mkBodyBuilder (\_ -> b)
