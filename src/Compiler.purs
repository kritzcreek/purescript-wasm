module Compiler where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Wasm.Builder as Builder
import Wasm.Syntax as S

testModule :: S.Module
testModule =
  let
    moduleRes = Builder.build do
      let { body, locals } = compileCExp thirtyFiveSquared
      fillBody <- Builder.declareFunc
        "ourMain"
        { arguments: [], results: [S.I32] }
        (Array.replicate locals S.I32)
      fillBody body
      Builder.declareExport "ourMain" "main" in
  case moduleRes of
    Left err -> unsafeCrashWith (show err)
    Right m -> m

data Expr
  = Literal Int
  | Var String
  | Addition Expr Expr
  | Multiplication Expr Expr

-- | let seven = 3 + 4 in
-- | let thirtyfive = seven * 5 in
-- | let two = 2 in
-- | thirtyfive * two

thirtyFiveSquared :: CExp
thirtyFiveSquared =
  { bindings:
    [ { name: "thirtyfive"
      , expr: Multiplication (Addition (Literal 3) (Literal 4)) (Literal 5)
      }
    , { name: "two"
      , expr: Literal 2
      }
    ]
  , body: Multiplication (Var "thirtyfive") (Var "two")
  }

type CExp
  = { bindings :: Array { name :: String, expr :: Expr }
    , body :: Expr
    }

type Env = Array String

compileExpr :: Env -> Expr -> S.Expr
compileExpr env = case _ of
  Literal x ->
    [ S.I32Const x ]
  Var v ->
    [ S.LocalGet (unsafePartial fromJust (Array.findIndex (_ == v) env)) ]
  Addition x y ->
    compileExpr env x <> compileExpr env y <> [ S.I32Add ]
  Multiplication x y ->
    compileExpr env x <> compileExpr env y <> [ S.I32Mul ]

compileCExp :: CExp -> { body :: S.Expr, locals :: Int }
compileCExp { bindings, body } =
  let env = map _.name bindings in
  let initialization = flip foldMapWithIndex bindings \ix { expr } ->
        compileExpr [] expr <> [ S.LocalSet ix ] in
  { locals: Array.length bindings
  , body: initialization <> compileExpr env body
  }
