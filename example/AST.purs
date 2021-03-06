module AST where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Op = Add | Sub | Mul | Div | Lt | Gt | Lte | Gte | Eq

derive instance genericOp :: Generic Op _
instance showOp :: Show Op where
  show x = genericShow x

data Expr a
  = IntLit Int
  | BoolLit Boolean
  | Var a
  | BinOp Op (Expr a) (Expr a)
  | If (Expr a) (Expr a) (Expr a)
  | Call a (Array (Expr a))

derive instance genericExpr :: Generic (Expr a) _
instance showExpr :: Show a => Show (Expr a) where
  show x = genericShow x

data Decl a
  = LetD a (Expr a)
  | ExprD (Expr a)

derive instance genericDecl :: Generic (Decl a) _
instance showDecl :: Show a => Show (Decl a) where
  show x = genericShow x

isLetD :: forall a. Decl a -> Boolean
isLetD = case _ of
  LetD _ _ -> true
  _ -> false

data Func a
  = Func a (Array a) (Array (Decl a))

derive instance genericFunc :: Generic (Func a) _
instance showFunc :: Show a => Show (Func a) where
  show x = genericShow x
