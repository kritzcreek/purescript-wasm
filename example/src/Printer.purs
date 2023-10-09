module Printer (printFuncs, printExpr, printDecl) where

import Prelude

import AST (Decl(..), Expr(..), Func(..), Op(..))
import Data.Array as Array
import Dodo (Doc, break, encloseEmptyAlt, flexAlt, flexGroup, indent, plainText, print, text, twoSpaces, (<+>), (</>))
import Dodo as D
import Dodo.Common as C

print' :: forall a. Doc a -> String
print' = print plainText twoSpaces

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

renderExpr :: forall a. Expr String -> Doc a
renderExpr = case _ of
  IntLit x ->
    text (show x)
  BoolLit x ->
    text (show x)
  Var v ->
    text v
  BinOp op l r ->
    renderExpr l <+> renderOp op <+> renderExpr r
  If cond t e ->
    text "if" <+> renderExpr cond
      <+> curlies (renderExpr t)
      <+> text "else"
      <+>
        curlies (renderExpr e)
  Call func args ->
    text func <> parens (commaSep (map renderExpr args))

renderDecl :: forall a. Decl String -> Doc a
renderDecl = case _ of
  LetD name expr ->
    (text "let" <+> text name <+> text "=") </> renderExpr expr
  ExprD expr ->
    renderExpr expr

renderFunc :: forall a. Func String -> Doc a
renderFunc (Func name params body) = do
  let headerD = text "fn" <+> text name
  let paramD = parens (commaSep (map text params))
  let bodyD = D.foldWithSeparator (text ";" <> break) (map renderDecl body)
  headerD <> paramD <+> curlies bodyD

curlies :: forall a. Doc a -> Doc a
curlies = flexGroup <<< encloseEmptyAlt open close (text "{}") <<< indent
  where
  open = text "{" <> break
  close = break <> text "}"

parens :: forall a. Doc a -> Doc a
parens = flexGroup <<< encloseEmptyAlt open close (text "()") <<< indent
  where
  open = flexAlt (text "(") (text "(" <> break)
  close = flexAlt (text ")") (break <> text ")")

commaSep :: forall a. Array (Doc a) -> Doc a
commaSep = flexGroup <<< Array.intercalate C.trailingComma
