module Ast where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)

data Op = Add | Sub | Mul | Div | Lt | Gt | Lte | Gte | Eq

derive instance genericOp :: Generic Op _
instance showOp :: Show Op where
  show x = genericShow x

data Lit = IntLit Int | BoolLit Boolean

derive instance Generic Lit _
instance Show Lit where
  show x = genericShow x

data Expr a
  = LitE Lit
  | VarE a
  | BinOpE Op (Expr a) (Expr a)
  | IfE (Expr a) (Expr a) (Expr a)
  | CallE a (Array (Expr a))
  | BlockE (Array (Decl a))

derive instance genericExpr :: Generic (Expr a) _
instance showExpr :: Show a => Show (Expr a) where
  show x = genericShow x

data Decl a
  = LetD a (Expr a)
  | SetD a (Expr a)
  | ExprD (Expr a)

derive instance genericDecl :: Generic (Decl a) _
instance showDecl :: Show a => Show (Decl a) where
  show x = genericShow x

isLetD :: forall a. Decl a -> Boolean
isLetD = case _ of
  LetD _ _ -> true
  _ -> false

type Func a =
  { name :: a
  , export :: Maybe String
  , params :: NonEmptyArray a
  , body :: Expr a
  }

data ValTy = I32

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

data Toplevel a
  = TopFunc (Func a)
  | TopLet a (Expr a)
  | TopImport a FuncTy String -- Name, Type, ExternalName

derive instance Generic (Toplevel a) _
instance Show a => Show (Toplevel a) where
  show x = genericShow x

type Program a = Array (Toplevel a)
