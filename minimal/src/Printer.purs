module Printer (printFuncs, printProgram, printExpr, printDecl) where

import Prelude

import Ast (Decl(..), Expr(..), Func, FuncTy(..), Lit(..), Op(..), Program, Toplevel(..), ValTy(..))
import Data.Array as Array
import Dodo (Doc, break, encloseEmptyAlt, flexGroup, indent, plainText, print, text, twoSpaces, (<+>), (</>))
import Dodo as D

print' :: forall a. Doc a -> String
print' = print plainText twoSpaces

printProgram :: forall a. (a -> String) -> Program a -> String
printProgram showVar toplevels =
  print' (Array.intercalate (break <> break) (map (renderToplevel showVar) toplevels))

printFuncs :: forall a. (a -> String) -> Array (Func a) -> String
printFuncs showVar funcs =
  print' (Array.intercalate (break <> break) (map (renderFunc showVar) funcs))

printExpr :: forall a. (a -> String) -> Expr a -> String
printExpr showVar expr =
  print' (renderExpr showVar expr)

printDecl :: forall a. (a -> String) -> Decl a -> String
printDecl showVar decl =
  print' (renderDecl showVar decl)

renderOp :: forall a. Op -> Doc a
renderOp = text <<< case _ of
  Add -> "+"
  Sub -> "-"
  Mul -> "*"
  Div -> "/"
  Lt -> "<"
  Gt -> ">"
  Lte -> "<="
  Gte -> ">="
  Eq -> "=="

renderLit :: forall a. Lit -> Doc a
renderLit = case _ of
  IntLit x ->
    text (show x)
  BoolLit x ->
    text (show x)

-- TODO: Prec based parenthesis
renderExpr :: forall a name. (name -> String) -> Expr name -> Doc a
renderExpr showVar = case _ of
  LitE lit ->
    renderLit lit
  VarE v ->
    text (showVar v)
  BinOpE op l r ->
    renderExpr showVar l <+> renderOp op <+> renderExpr showVar r
  IfE cond t e ->
    text "if" <+> renderExpr showVar cond
      <+> renderExpr showVar t
      <+> text "else"
      <+> renderExpr showVar e
  CallE func arg ->
    renderExpr showVar func <+> renderExpr showVar arg
  BlockE body ->
    curlies (D.foldWithSeparator (text ";" <> break) (map (renderDecl showVar) body))

renderDecl :: forall a name. (name -> String) -> Decl name -> Doc a
renderDecl showVar = case _ of
  LetD name expr ->
    (text "let" <+> text (showVar name) <+> text "=") </> renderExpr showVar expr
  SetD name expr ->
    (text "set" <+> text (showVar name) <+> text "=") </> renderExpr showVar expr
  ExprD expr ->
    renderExpr showVar expr

renderFunc :: forall a name. (name -> String) -> Func name -> Doc a
renderFunc showVar func = do
  let headerD = text (showVar func.name)
  let paramD = D.words (map (text <<< showVar) func.params)
  let bodyD = renderExpr showVar func.body
  headerD <+> paramD <+> text "=" <+> bodyD

renderValTy :: forall a. ValTy -> Doc a
renderValTy = case _ of
  I32 -> text "i32"

renderFuncTy :: forall a. FuncTy -> Doc a
renderFuncTy = case _ of
  FuncTy arguments result ->
    parens (D.foldWithSeparator (text "," <> D.space) (map renderValTy arguments)) <+> text "->" <+> renderValTy result

renderToplevel :: forall a name. (name -> String) -> Toplevel name -> Doc a
renderToplevel showVar = case _ of
  TopFunc func -> renderFunc showVar func
  TopLet name init ->
    (text "let" <+> text (showVar name) <+> text "=") </> renderExpr showVar init <> text ";"
  TopImport name ty externalName ->
    text "import"
      <+> text (showVar name)
      <+> text ":"
      <+> renderFuncTy ty
      <+> text "from"
      <+> text externalName

curlies :: forall a. Doc a -> Doc a
curlies = flexGroup <<< encloseEmptyAlt open close (text "{}") <<< indent
  where
  open = text "{" <> break
  close = break <> text "}"

parens :: forall a. Doc a -> Doc a
parens = encloseEmptyAlt (text "(") (text ")") (text "()")
