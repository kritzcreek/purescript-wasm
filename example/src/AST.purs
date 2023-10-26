module Ast where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)

data Op = Add | Sub | Mul | Div | Lt | Gt | Lte | Gte | Eq

derive instance genericOp :: Generic Op _
instance showOp :: Show Op where
  show x = genericShow x

data Lit = IntLit Int | BoolLit Boolean | FloatLit Number

derive instance Generic Lit _
instance Show Lit where
  show x = genericShow x

type Expr note name = { note :: note, expr :: Expr' note name }

data Expr' note name
  = LitE Lit
  | VarE name
  | ArrayE (Array (Expr note name))
  | ArrayIdxE (Expr note name) (Expr note name)
  | BinOpE Op (Expr note name) (Expr note name)
  | IfE (Expr note name) (Expr note name) (Expr note name)
  | CallE name (Array (Expr note name))
  | BlockE (Array (Decl note name))

derive instance genericExpr :: Generic (Expr' note a) _
instance showExpr :: (Show note, Show a) => Show (Expr' note a) where
  show x = genericShow x

data Decl note name
  = LetD name (Expr note name)
  | SetD name (Expr note name)
  | ExprD (Expr note name)

derive instance genericDecl :: Generic (Decl note name) _
instance showDecl :: (Show note, Show name) => Show (Decl note name) where
  show x = genericShow x

isLetD :: forall note name. Decl note name -> Boolean
isLetD = case _ of
  LetD _ _ -> true
  _ -> false

type Func note name =
  { name :: name
  , export :: Maybe String
  , params :: Array { name :: name, ty :: ValTy }
  , returnTy :: ValTy
  , body :: Expr note name
  }

funcTyOf :: forall note name. Func note name -> FuncTy
funcTyOf { params, returnTy } = FuncTy (map _.ty params) returnTy

data Toplevel note name
  = TopFunc (Func note name)
  | TopLet name (Expr note name)
  | TopImport name FuncTy String -- Name, Type, ExternalName

derive instance Generic (Toplevel note name) _
instance (Show note, Show name) => Show (Toplevel note name) where
  show x = genericShow x

type Program note name = Array (Toplevel note name)

-- Types

data ValTy = TyI32 | TyF32 | TyBool | TyUnit | TyArray ValTy

derive instance Eq ValTy
derive instance Generic ValTy _
instance Show ValTy where
  show x = genericShow x

-- Potentially multi value returns?
data FuncTy = FuncTy (Array ValTy) ValTy

derive instance Eq FuncTy
derive instance Generic FuncTy _
instance Show FuncTy where
  show x = genericShow x
