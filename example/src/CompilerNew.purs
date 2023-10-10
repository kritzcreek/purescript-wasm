module CompilerNew where

import Prelude

import AstNew as AST
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (traverse, traverse_)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import PrinterNew as Printer
import Wasm.Syntax as S
import WasmBuilder as Builder

type Builder = Builder.Builder String

type Locals = Map String S.LocalIdx

compileFuncs :: Array (AST.Func String) -> S.Module
compileFuncs funcs = Builder.build' do
  _rts <- declareRts
  fills <- traverse declareFunc funcs
  traverse_ implFunc fills
  Builder.declareExport "main" "main"

type Rts = { alloc :: S.Instruction }

declareRts :: Builder Rts
declareRts = do
  watermark <- Builder.declareGlobal
    "watermark"
    { mutability: S.Var, type: (S.NumType S.I32) }
    [ S.I32Const 256 ]
  allocFill <- Builder.declareFunc "alloc" { arguments: [ S.NumType S.I32 ], results: [ S.NumType S.I32 ] }
  let
    { locals, body } = Builder.bodyBuild [ "allocSize" ] do
      allocSize <- Builder.getParam "allocSize"
      Builder.addInstructions
        [ S.GlobalGet watermark
        , S.GlobalGet watermark
        , allocSize.get
        , S.I32Add
        , S.GlobalSet watermark
        ]
  allocFill locals body
  callAlloc <- Builder.callFunc "alloc"

  pure { alloc: callAlloc }

compileOp :: AST.Op -> S.Instruction
compileOp = case _ of
  AST.Add -> S.I32Add
  AST.Sub -> S.I32Sub
  AST.Mul -> S.I32Mul
  AST.Div -> S.I32Div_s
  AST.Lt -> S.I32Lt_s
  AST.Gt -> S.I32Gt_s
  AST.Lte -> S.I32Le_s
  AST.Gte -> S.I32Ge_s
  AST.Eq -> S.I32Eq

lookupLocal :: String -> Locals -> S.LocalIdx
lookupLocal x ls =
  unsafePartial fromJust (Map.lookup x ls)

compileLit :: AST.Lit -> Builder (Array S.Instruction)
compileLit = case _ of
  AST.IntLit x -> pure [ S.I32Const x ]
  AST.BoolLit b -> pure [ if b then S.I32Const 1 else S.I32Const 0 ]

compileExpr :: Locals -> AST.Expr String -> Builder (Array S.Instruction)
compileExpr locals = case _ of
  AST.LitE lit -> compileLit lit
  AST.VarE x -> pure [ S.LocalGet (lookupLocal x locals) ]
  AST.BinOpE op l r -> ado
    l' <- compileExpr locals l
    r' <- compileExpr locals r
    in l' <> r' <> [ compileOp op ]
  AST.IfE cond t e -> ado
    cond' <- compileExpr locals cond
    t' <- compileExpr locals t
    e' <- compileExpr locals e
    in cond' <> [ S.If (S.BlockValType (Just (S.NumType S.I32))) t' e' ]
  AST.CallE func arg -> do
    { fn, args } <- case unfoldCall func [arg] of
      Nothing -> liftEffect (throw ("Can't unfold " <> Printer.printExpr (AST.CallE func arg)))
      Just r -> pure r
    args' <- traverse (compileExpr locals) args
    call <- Builder.callFunc fn
    pure (Array.fold args' <> [ call ])
  AST.BlockE _body -> ado
    in []

unfoldCall :: forall a. AST.Expr a -> Array (AST.Expr a) -> Maybe { fn :: a, args :: Array (AST.Expr a) }
unfoldCall f args = case f of
  AST.VarE fn -> Just { fn, args }
  AST.CallE newF newArg -> unfoldCall newF ([newArg] <> args)
  _ -> Nothing

compileBody
  :: Locals
  -> Array (AST.Decl String)
  -> Builder { expr :: Array S.Instruction, locals :: Array S.ValType }
compileBody params decls =
  case Array.unsnoc decls of
    Just { init, last: AST.ExprD expr } -> do
      { instrs, locals, newLocals } <-
        Array.foldM go { locals: params, instrs: [], newLocals: [] } init
      result <- compileExpr locals expr
      pure { expr: instrs <> result, locals: newLocals }
    _ ->
      unsafeCrashWith "Function body must end in an expression."
  where
  go
    :: { locals :: Locals, instrs :: S.Expr, newLocals :: Array S.ValType }
    -> AST.Decl String
    -> Builder { locals :: Locals, instrs :: S.Expr, newLocals :: Array S.ValType }
  go { locals, instrs, newLocals } = case _ of
    AST.ExprD expr -> do
      is <- compileExpr locals expr
      pure { locals, instrs: instrs <> is <> [ S.Drop ], newLocals }
    AST.LetD n e -> ado
      let index = Map.size locals
      is <- compileExpr locals e
      in
        { locals: Map.insert n index locals
        , instrs: instrs <> is <> [ S.LocalSet index ]
        , newLocals: Array.snoc newLocals (S.NumType S.I32)
        }

declareFunc
  :: AST.Func String
  -> Builder
       { fill :: Array S.ValType -> S.Expr -> Builder Unit
       , func :: AST.Func String
       }
declareFunc func@(AST.Func name params _) = do
  fill <- Builder.declareFunc
    name
    { arguments: map (const (S.NumType S.I32)) (NEA.toArray params)
    , results: [ S.NumType S.I32 ]
    }
  pure { fill, func }

implFunc
  :: { fill :: Array S.ValType -> S.Expr -> Builder Unit
     , func :: AST.Func String
     }
  -> Builder Unit
implFunc { fill, func: AST.Func _ params body } =
  case body of
    AST.BlockE decls -> do
      let params' = foldlWithIndex (\ix xs name -> Map.insert name ix xs) Map.empty params
      { expr, locals } <- compileBody params' decls
      fill locals expr
    e -> liftEffect (throw ("invalid function body: " <> Printer.printExpr e))
