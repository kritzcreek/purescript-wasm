module PrinterNew (printFuncs, printExpr, printDecl) where

import Prelude

import AstNew (Decl(..), Expr(..), Func(..), Lit(..), Op(..))
import Data.Array as Array
import Data.Array.NonEmpty as NEA
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
  ExprD expr ->
    renderExpr expr

renderFunc :: forall a. Func String -> Doc a
renderFunc (Func name params body) = do
  let headerD = text name
  let paramD = D.words (map text params)
  let bodyD = renderExpr body
  headerD <+> paramD <+> text "=" <+> bodyD

curlies :: forall a. Doc a -> Doc a
curlies = flexGroup <<< encloseEmptyAlt open close (text "{}") <<< indent
  where
  open = text "{" <> break
  close = break <> text "}"

-- parens :: forall a. Doc a -> Doc a
-- parens = flexGroup <<< encloseEmptyAlt open close (text "()") <<< indent
--   where
--   open = flexAlt (text "(") (text "(" <> break)
--   close = flexAlt (text ")") (break <> text ")")

-- commaSep :: forall a. Array (Doc a) -> Doc a
-- commaSep = flexGroup <<< Array.intercalate C.trailingComma
