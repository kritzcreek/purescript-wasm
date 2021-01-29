module Wasm.Builder
  ( Builder
  , BuildError(..)
  , build
  , build'
  , declareType
  , declareGlobal
  , declareFunc
  , declareExport
  , callFunc
  ) where

import Prelude

import Control.Monad.Reader (ReaderT(..), runReaderT)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafeCrashWith)
import Record as Record
import Wasm.Syntax (Export, ExportDesc(..), Expr, Func, FuncIdx, FuncType, Global, GlobalIdx, GlobalType, Instruction(..), LocalIdx, Memory, Module, Name, TypeIdx, ValType, emptyModule)

-- - Define functions
-- - Define globals
-- - Declare memories (export/import?)
-- - Declare start symbol?
-- - Declare tables and their elem sections
-- - Declare data sections? (Maybe together with the memory declaration)


type FuncData =
  { index :: FuncIdx
  , type :: TypeIdx
  , locals :: Maybe (Array ValType)
  , body :: Maybe Expr
  }

type GlobalData =
  { index :: GlobalIdx
  , type :: GlobalType
  -- TODO: Do we need to delay initialization of the Expr?
  , init :: Expr
  }

type Env name =
  { funcs :: Ref (Map name FuncData)
  , funcsSupply :: Ref Int
  , globals :: Ref (Map name GlobalData)
  , globalsSupply :: Ref Int
  , types :: Ref (Array FuncType)
  , memory :: Ref (Maybe Memory)
  , exports :: Ref (Array Export)
  }

initialEnv :: forall name. Effect (Env name)
initialEnv = ado
  funcs <- Ref.new Map.empty
  funcsSupply <- Ref.new (-1)
  globals <- Ref.new Map.empty
  globalsSupply <- Ref.new (-1)
  types <- Ref.new []
  memory <- Ref.new Nothing
  exports <- Ref.new []
  in { funcs, funcsSupply, globals, globalsSupply, types, memory, exports }

newtype Builder name a = Builder (ReaderT (Env name) Effect a)

derive newtype instance functorBuilder :: Functor (Builder name)
derive newtype instance applyBuilder :: Apply (Builder name)
derive newtype instance applicativeBuilder :: Applicative (Builder name)
derive newtype instance bindBuilder :: Bind (Builder name)
derive newtype instance monadBuilder :: Monad (Builder name)

mkBuilder :: forall name a. (Env name -> Effect a) -> Builder name a
mkBuilder act = Builder (ReaderT act)

declareType :: forall name. FuncType -> Builder name TypeIdx
declareType ty = mkBuilder \{ types } -> do
  ts <- Ref.read types
  case Array.findIndex (_ == ty) ts of
    Nothing -> do
      Ref.write (Array.snoc ts ty) types
      pure (Array.length ts)
    Just ix ->
      pure ix

nextGlobalIdx :: forall name. Builder name GlobalIdx
nextGlobalIdx =
  mkBuilder \{ globalsSupply } -> Ref.modify (_ + 1) globalsSupply

declareGlobal ::
  forall name.
  Show name =>
  Ord name =>
  name ->
  GlobalType ->
  Expr ->
  Builder name GlobalIdx
declareGlobal name ty init = do
  index <- nextGlobalIdx
  mkBuilder \{ globals, globalsSupply } -> do
    gs <- Ref.read globals
    when (Map.member name gs) do
      throw ("double declaring global: " <> show name)
    Ref.modify_ (Map.insert name { index, type: ty, init }) globals
    pure index

nextFuncIdx :: forall name. Builder name FuncIdx
nextFuncIdx =
  mkBuilder \{ funcsSupply } -> Ref.modify (_ + 1) funcsSupply

declareFunc ::
  forall name.
  Show name =>
  Ord name =>
  name ->
  FuncType ->
  Builder name (Array ValType -> Expr -> Builder name Unit)
declareFunc name ty = do
  tyIndex <- declareType ty
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

data BuildError name
  = MissingBody name

instance showBuildError :: Show name => Show (BuildError name) where
  show = case _ of
    MissingBody name -> "Missing body for: " <> show name

buildFuncs ::
  forall name.
  Show name =>
  Env name ->
  Effect (Either (BuildError name) (Array Func))
buildFuncs { funcs, types } = do
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

buildGlobals ::
  forall name.
  Show name =>
  Env name ->
  Effect (Array Global)
buildGlobals { globals } = do
  gs <- map Map.toUnfoldable (Ref.read globals)
  let sortedGlobals = Array.sortWith (_.index <<< Tuple.snd) gs
  pure (map (Record.delete (SProxy :: _ "index") <<< Tuple.snd) sortedGlobals)

buildModule ::
  forall name.
  Show name =>
  Env name ->
  Effect (Either (BuildError name) Module)
buildModule env@{ types, memory, exports } = do
  ts <- Ref.read types
  wasmGlobals <- buildGlobals env
  mem <- Ref.read memory
  exps <- Ref.read exports
  buildFuncs env <#> case _ of
    Left err ->
      Left err
    Right wasmFuncs ->
      Right
        (emptyModule
         { funcs = wasmFuncs
         , globals = wasmGlobals
         , types = ts
         , memories = Array.fromFoldable mem
         , exports = exps
         })

build :: forall name a. Show name => Builder name a -> Either (BuildError name) Module
build (Builder b) = unsafePerformEffect do
  env <- initialEnv
  _ <- runReaderT b env
  buildModule env

build' :: forall name a. Show name => Builder name a -> Module
build' b = case build b of
  Right m -> m
  Left err -> unsafeCrashWith (show err)

type LocalData = { index :: LocalIdx, type :: ValType }

type BodyEnv name =
  { params :: Array name
  , locals :: Ref (Map name LocalData)
  , localsSupply :: Ref Int
  , instrs :: Ref (Array Expr)
  }

newtype BodyBuilder name a = BodyBuilder (ReaderT (BodyEnv name) Effect a)

derive newtype instance functorBodyBuilder :: Functor (BodyBuilder name)
derive newtype instance applyBodyBuilder :: Apply (BodyBuilder name)
derive newtype instance applicativeBodyBuilder :: Applicative (BodyBuilder name)
derive newtype instance bindBodyBuilder :: Bind (BodyBuilder name)
derive newtype instance monadBodyBuilder :: Monad (BodyBuilder name)

initialBodyEnv :: forall name. Array name -> Effect (BodyEnv name)
initialBodyEnv params = ado
  locals <- Ref.new Map.empty
  instrs <- Ref.new []
  localsSupply <- Ref.new (-1)
  in { params, locals, localsSupply, instrs }

nextLocalIdx :: forall name. BodyBuilder name LocalIdx
nextLocalIdx =
  mkBodyBuilder \{ localsSupply } -> Ref.modify (_ + 1) localsSupply

mkBodyBuilder :: forall name a. (BodyEnv name -> Effect a) -> BodyBuilder name a
mkBodyBuilder act = BodyBuilder (ReaderT act)

bodyBuild ::
  forall name a.
  Show name =>
  Array name ->
  BodyBuilder name a ->
  Either (BuildError name) { locals :: Array ValType, body :: Expr }
bodyBuild params (BodyBuilder b) = unsafePerformEffect do
  env <- initialBodyEnv params
  _ <- runReaderT b env
  buildBody env

buildBody ::
  forall name.
  Show name =>
  BodyEnv name ->
  Effect (Either (BuildError name) { locals :: Array ValType, body :: Expr })
buildBody { locals, instrs } = do
  ls <- map Map.toUnfoldable (Ref.read locals)
  let sortedLocals = Array.sortWith (_.index <<< Tuple.snd) ls
  instrs' <- Ref.read instrs
  pure (Right { locals: map (_.type <<< Tuple.snd) sortedLocals, body: Array.concat instrs' })

newLocal ::
  forall name.
  Ord name =>
  name ->
  ValType ->
  BodyBuilder name { set :: Instruction, get :: Instruction }
newLocal name ty = do
  index <- nextLocalIdx
  mkBodyBuilder \{ locals } -> do
    Ref.modify_ (Map.insert name { type: ty, index }) locals
    pure { set: LocalSet index, get: LocalGet index }

addInstructions ::
  forall name.
  Array Instruction ->
  BodyBuilder name Unit
addInstructions new = mkBodyBuilder \{ instrs } ->
  Ref.modify_ (flip Array.snoc new) instrs
