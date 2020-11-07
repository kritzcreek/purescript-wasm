module Compiler where

import Prelude

import Data.Array as Array
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Maybe (Maybe(..), fromJust)
import Partial.Unsafe (unsafePartial)
import Wasm.Syntax as S

testModule :: S.Module
testModule = S.emptyModule
  { types = [ { arguments: [], results: [ S.I32 ] } ]
  , funcs = [ tmpFunction thirtyFiveSquared ]
  , exports = [ { name: "main", desc: S.ExportFunc 0 } ]
  }

tmpFunction :: CExp -> S.Func
tmpFunction exp =
  let { body, locals } = compileCExp exp in
  { type: 0, locals: Array.replicate locals S.I32, body }

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
