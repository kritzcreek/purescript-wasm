module Ast where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)

data Op
  = Add
  | Sub
  | Mul
  | Div
  | Lt
  | Gt
  | Lte
  | Gte
  | Eq
  | Neq
  | And
  | Or

derive instance genericOp :: Generic Op _
instance showOp :: Show Op where
  show x = genericShow x

data Lit
  = IntLit Int
  | BoolLit Boolean
  | FloatLit Number
  | TextLit String

derive instance Generic Lit _
instance Show Lit where
  show x = genericShow x

type Expr note name = { note :: note, expr :: Expr' note name }

data Expr' note name
  = LitE Lit
  | VarE name
  | ArrayE (Array (Expr note name))
  | ArrayIdxE (Expr note name) (Expr note name)
  | StructE name (Array { name :: name, expr :: (Expr note name) })
  | StructIdxE (Expr note name) name
  | BinOpE Op (Expr note name) (Expr note name)
  | IfE (Expr note name) (Expr note name) (Expr note name)
  | CallE name (Array (Expr note name))
  | IntrinsicE Intrinsic (Array (Expr note name))
  | BlockE (Array (Decl note name))

derive instance genericExpr :: Generic (Expr' note a) _
instance showExpr :: (Show note, Show a) => Show (Expr' note a) where
  show x = genericShow x

data Decl note name
  = LetD name (Expr note name)
  | SetD (SetTarget note name) (Expr note name)
  | ExprD (Expr note name)
  | WhileD (Expr note name) (Expr note name)

derive instance genericDecl :: Generic (Decl note name) _
instance showDecl :: (Show note, Show name) => Show (Decl note name) where
  show x = genericShow x

data SetTarget note name
  = VarST note name
  | ArrayIdxST note name (Expr note name)
  | StructIdxST note name name

derive instance Generic (SetTarget note name) _
instance (Show note, Show name) => Show (SetTarget note name) where
  show x = genericShow x

isLetD :: forall note name. Decl note name -> Boolean
isLetD = case _ of
  LetD _ _ -> true
  _ -> false

type Func note name =
  { name :: name
  , export :: Maybe String
  , params :: Array { name :: name, ty :: Ty name }
  , returnTy :: Ty name
  , body :: Expr note name
  }

funcTyOf :: forall note name. Func note name -> FuncTy name
funcTyOf { params, returnTy } = FuncTy (map _.ty params) returnTy

data Toplevel note name
  = TopFunc (Func note name)
  | TopLet name (Expr note name)
  | TopImport name (FuncTy name) String -- Name, Type, ExternalName
  | TopStruct name (Array { name :: name, ty :: Ty name })

derive instance Generic (Toplevel note name) _
instance (Show note, Show name) => Show (Toplevel note name) where
  show x = genericShow x

type Program note name = Array (Toplevel note name)

-- Types

data Ty name = TyI32 | TyF32 | TyBool | TyText | TyUnit | TyArray (Ty name) | TyCons name

derive instance Eq name => Eq (Ty name)
derive instance Ord name => Ord (Ty name)
derive instance Generic (Ty name) _
instance Show name => Show (Ty name) where
  show x = genericShow x

-- Potentially multi value returns?
data FuncTy name = FuncTy (Array (Ty name)) (Ty name)

derive instance Eq name => Eq (FuncTy name)
derive instance Ord name => Ord (FuncTy name)
derive instance Generic (FuncTy name) _
instance Show name => Show (FuncTy name) where
  show x = genericShow x

data Intrinsic
  = ArrayNew
  | ArrayLen

derive instance Eq Intrinsic
derive instance Ord Intrinsic
derive instance Generic Intrinsic _
instance Show Intrinsic where
  show = case _ of
    ArrayNew -> "@array_new"
    ArrayLen -> "@array_len"
