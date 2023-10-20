module Printer (printFuncs, printToplevels, printExpr, printDecl) where

import Prelude

import Ast (Decl(..), Expr(..), Func(..), FuncTy(..), Lit(..), Op(..), Toplevel(..), ValTy(..))
import Data.Array as Array
import Dodo (Doc, break, encloseEmptyAlt, flexGroup, indent, plainText, print, text, twoSpaces, (<+>), (</>))
import Dodo as D

print' :: forall a. Doc a -> String
print' = print plainText twoSpaces

printToplevels :: Array (Toplevel String) -> String
printToplevels toplevels =
  print' (Array.intercalate (break <> break) (map renderToplevel toplevels))

printFuncs :: Array (Func String) -> String
printFuncs funcs =
  print' (Array.intercalate (break <> break) (map renderFunc funcs))

printExpr :: Expr String -> String
printExpr expr =
  print' (renderExpr expr)

printDecl :: Decl String -> String
printDecl decl =
  print' (renderDecl decl)

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
renderExpr :: forall a. Expr String -> Doc a
renderExpr = case _ of
  LitE lit ->
    renderLit lit
  VarE v ->
    text v
  BinOpE op l r ->
    renderExpr l <+> renderOp op <+> renderExpr r
  IfE cond t e ->
    text "if" <+> renderExpr cond
      <+> renderExpr t
      <+> text "else"
      <+> renderExpr e
  CallE func arg ->
    renderExpr func <+> renderExpr arg
  BlockE body ->
    curlies (D.foldWithSeparator (text ";" <> break) (map renderDecl body))

renderDecl :: forall a. Decl String -> Doc a
renderDecl = case _ of
  LetD name expr ->
    (text "let" <+> text name <+> text "=") </> renderExpr expr
  SetD name expr ->
    (text "set" <+> text name <+> text "=") </> renderExpr expr
  ExprD expr ->
    renderExpr expr

renderFunc :: forall a. Func String -> Doc a
renderFunc (Func name params body) = do
  let headerD = text name
  let paramD = D.words (map text params)
  let bodyD = renderExpr body
  headerD <+> paramD <+> text "=" <+> bodyD

renderValTy :: forall a. ValTy -> Doc a
renderValTy = case _ of
  I32 -> text "i32"

renderFuncTy :: forall a. FuncTy -> Doc a
renderFuncTy = case _ of
  FuncTy arguments result ->
    parens (D.foldWithSeparator (text "," <> D.space) (map renderValTy arguments)) <+> text "->" <+> renderValTy result

renderToplevel :: forall a. Toplevel String -> Doc a
renderToplevel = case _ of
  TopFunc func -> renderFunc func
  TopLet name init ->
    (text "let" <+> text name <+> text "=") </> renderExpr init <> text ";"
  TopImport name ty externalName ->
    text "import"
    <+> text name
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
