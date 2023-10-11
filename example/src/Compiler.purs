module Compiler (compileFuncs) where

import Prelude

import Ast as Ast
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.FoldableWithIndex (foldlWithIndex)
import Data.List as List
import Data.List.NonEmpty as NEL
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Traversable (traverse, traverse_)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Printer as Printer
import Wasm.Syntax as S
import WasmBuilder (bodyBuild)
import WasmBuilder as Builder

type Builder = Builder.Builder String
type BodyBuilder = Builder.BodyBuilder String

type Scope = NEL.NonEmptyList (Map String S.LocalIdx)

withBlock :: forall a. Scope -> (Scope -> a) -> a
withBlock s f = f (NEL.cons Map.empty s)

lookupScope :: String -> Scope -> S.LocalIdx
lookupScope n s = unsafePartial Maybe.fromJust (List.findMap (\blockScope -> Map.lookup n blockScope) s)

addLocal :: String -> S.LocalIdx -> Scope -> Scope
addLocal n idx scope = do
  let { head, tail } = NEL.uncons scope
  NEL.cons' (Map.insert n idx head) tail

compileFuncs :: Array (Ast.Func String) -> S.Module
compileFuncs funcs = Builder.build' do
  fills <- traverse declareFunc funcs
  traverse_ implFunc fills
  Builder.declareExport "main" "main"

compileOp :: Ast.Op -> S.Instruction
compileOp = case _ of
  Ast.Add -> S.I32Add
  Ast.Sub -> S.I32Sub
  Ast.Mul -> S.I32Mul
  Ast.Div -> S.I32Div_s
  Ast.Lt -> S.I32Lt_s
  Ast.Gt -> S.I32Gt_s
  Ast.Lte -> S.I32Le_s
  Ast.Gte -> S.I32Ge_s
  Ast.Eq -> S.I32Eq

compileLit :: Ast.Lit -> Array S.Instruction
compileLit = case _ of
  Ast.IntLit x -> [ S.I32Const x ]
  Ast.BoolLit b -> [ if b then S.I32Const 1 else S.I32Const 0 ]

compileExpr :: Scope -> Ast.Expr String -> BodyBuilder (Array S.Instruction)
compileExpr scope = case _ of
  Ast.LitE lit -> pure (compileLit lit)
  Ast.VarE x -> pure [ S.LocalGet (lookupScope x scope) ]
  Ast.BinOpE op l r -> ado
    l' <- compileExpr scope l
    r' <- compileExpr scope r
    in l' <> r' <> [ compileOp op ]
  Ast.IfE cond t e -> ado
    cond' <- compileExpr scope cond
    t' <- compileExpr scope t
    e' <- compileExpr scope e
    in cond' <> [ S.If (S.BlockValType (Just (S.NumType S.I32))) t' e' ]
  Ast.CallE func arg -> do
    let
      { fn, args } = case unfoldCall func [ arg ] of
        Nothing -> unsafeCrashWith ("Can't unfold " <> Printer.printExpr (Ast.CallE func arg))
        Just r -> r
    args' <- traverse (compileExpr scope) args
    call <- Builder.liftBuilder (Builder.callFunc fn)
    pure (Array.fold args' <> [ call ])
  Ast.BlockE body -> compileBlock scope body

unfoldCall :: forall a. Ast.Expr a -> Array (Ast.Expr a) -> Maybe { fn :: a, args :: Array (Ast.Expr a) }
unfoldCall f args = case f of
  Ast.VarE fn -> Just { fn, args }
  Ast.CallE newF newArg -> unfoldCall newF ([ newArg ] <> args)
  _ -> Nothing

compileBlock
  :: Scope
  -> Array (Ast.Decl String)
  -> BodyBuilder (Array S.Instruction)
compileBlock outer decls = withBlock outer \scope -> do
  case Array.unsnoc decls of
    Just { init, last: Ast.ExprD expr } -> do
      { instrs, scope: scope' } <- Array.foldM go { scope, instrs: [] } init
      result <- compileExpr scope' expr
      pure (instrs <> result)
    _ ->
      unsafeCrashWith "Function body must end in an expression."
  where
  go
    :: { scope :: Scope, instrs :: S.Expr }
    -> Ast.Decl String
    -> BodyBuilder { scope :: Scope, instrs :: S.Expr }
  go { scope, instrs } = case _ of
    Ast.ExprD expr -> do
      is <- compileExpr scope expr
      pure { scope, instrs: instrs <> is <> [ S.Drop ] }
    Ast.LetD n e -> do
      var <- Builder.newLocal (S.NumType S.I32)
      let scope' = addLocal n var scope
      is <- compileExpr scope' e
      pure
        { scope: scope'
        , instrs: instrs <> is <> [ S.LocalSet var ]
        }

declareFunc
  :: Ast.Func String
  -> Builder
       { fill :: Array S.ValType -> S.Expr -> Builder Unit
       , func :: Ast.Func String
       }
declareFunc func@(Ast.Func name params _) = do
  fill <- Builder.declareFunc
    name
    { arguments: map (const (S.NumType S.I32)) (NEA.toArray params)
    , results: [ S.NumType S.I32 ]
    }
  pure { fill, func }

implFunc
  :: { fill :: Array S.ValType -> S.Expr -> Builder Unit
     , func :: Ast.Func String
     }
  -> Builder Unit
implFunc { fill, func: Ast.Func _ params body } = do
  let params' = foldlWithIndex (\ix xs name -> Map.insert name ix xs) Map.empty params
  let initialScope = NEL.singleton params'
  let paramTys = NEA.toArray (map (const (S.NumType S.I32)) params)
  fnBody <- bodyBuild paramTys (compileExpr initialScope body)
  fill fnBody.locals fnBody.result
