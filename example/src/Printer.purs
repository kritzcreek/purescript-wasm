module Printer (printFuncs, printProgram, printExpr, printDecl, renderAll, renderNone) where

import Prelude

import Ast (Decl(..), Expr, Expr'(..), Func, FuncTy(..), Lit(..), Op(..), Program, Toplevel(..), ValTy(..))
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe as Maybe
import Dodo (Doc, break, encloseEmptyAlt, flexGroup, indent, plainText, print, text, twoSpaces, (<+>), (</>))
import Dodo as D
import Rename as Rename
import Types as Types

type RenderOptions note name a =
  { renderNote :: note -> Doc a -> Doc a
  , renderName :: name -> Doc a
  }

renderTyNote :: forall a. Types.Ty -> Doc a -> Doc a
renderTyNote ty doc = parens (doc <+> text ":" <+> text (Types.showTy ty))

renderAll :: forall a. Map Rename.Var String -> RenderOptions Types.Ty Rename.Var a
renderAll nameMap =
  { renderNote: renderTyNote
  , renderName: \v -> text (Maybe.maybe "$UNKNOWN" (\n -> show v <> n) (Map.lookup v nameMap))
  }

renderNone :: forall a note name. Show name => RenderOptions note name a
renderNone =
  { renderNote: \_ doc -> doc
  , renderName: text <<< show
  }

print' :: forall a. Doc a -> String
print' = print plainText twoSpaces

printProgram :: forall note name a. RenderOptions note name a -> Program note name -> String
printProgram renderOptions toplevels =
  print' (Array.intercalate (break <> break) (map (renderToplevel renderOptions) toplevels))

printFuncs :: forall note name a. RenderOptions note name a -> Array (Func note name) -> String
printFuncs renderOptions funcs =
  print' (Array.intercalate (break <> break) (map (renderFunc renderOptions) funcs))

printExpr :: forall note name a. RenderOptions note name a -> Expr note name -> String
printExpr renderOptions expr =
  print' (renderExpr renderOptions expr)

printDecl :: forall note name a. RenderOptions note name a -> Decl note name -> String
printDecl renderOptions decl =
  print' (renderDecl renderOptions decl)

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
  FloatLit x ->
    text (show x)
  BoolLit x ->
    text (show x)

-- TODO: Prec based parenthesis
renderExpr :: forall a note name. RenderOptions note name a -> Expr note name -> Doc a
renderExpr renderOptions expr = renderOptions.renderNote expr.note case expr.expr of
  LitE lit ->
    renderLit lit
  VarE v ->
    renderOptions.renderName v
  BinOpE op l r ->
    renderExpr renderOptions l <+> renderOp op <+> renderExpr renderOptions r
  IfE cond t e ->
    text "if" <+> renderExpr renderOptions cond
      <+> renderExpr renderOptions t
      <+> text "else"
      <+> renderExpr renderOptions e
  CallE func args ->
    renderOptions.renderName func <> parensIndent (D.foldWithSeparator (text "," <> D.spaceBreak) (map (renderExpr renderOptions) args))
  BlockE body ->
    curlies (D.foldWithSeparator (text ";" <> break) (map (renderDecl renderOptions) body))
  ArrayE elements ->
    brackets (D.foldWithSeparator (text "," <> break) (map (renderExpr renderOptions) elements))

renderDecl :: forall a note name. RenderOptions note name a -> Decl note name -> Doc a
renderDecl renderOptions = case _ of
  LetD name expr ->
    (text "let" <+> renderOptions.renderName name <+> text "=") </> renderExpr renderOptions expr
  SetD name expr ->
    (text "set" <+> renderOptions.renderName name <+> text "=") </> renderExpr renderOptions expr
  ExprD expr ->
    renderExpr renderOptions expr

renderFunc :: forall a note name. RenderOptions note name a -> Func note name -> Doc a
renderFunc renderOptions func = do
  let
    headerD = text "fn" <+> renderOptions.renderName func.name
    paramD =
      parensIndent (D.foldWithSeparator (text "," <> D.spaceBreak) (map renderParam func.params))
    returnTyD =
      if func.returnTy /= TyUnit then
        D.space <> text ":" <+> renderValTy func.returnTy
      else
        mempty
    bodyD = renderExpr renderOptions func.body
  (headerD <+> paramD <> returnTyD <+> text "=") </> indent bodyD
  where
  renderParam { name, ty } = renderOptions.renderName name <+> text ":" <+> renderValTy ty

renderValTy :: forall a. ValTy -> Doc a
renderValTy = case _ of
  TyI32 -> text "i32"
  TyF32 -> text "f32"
  TyBool -> text "bool"
  TyUnit -> text "()"
  TyArray t -> text "[" <> renderValTy t <> text "]"

renderFuncTy :: forall a. FuncTy -> Doc a
renderFuncTy = case _ of
  FuncTy arguments result ->
    parens (D.foldWithSeparator (text "," <> D.space) (map renderValTy arguments)) <+> text "->" <+> renderValTy result

renderToplevel :: forall a note name. RenderOptions note name a -> Toplevel note name -> Doc a
renderToplevel renderOptions = case _ of
  TopFunc func -> renderFunc renderOptions func
  TopLet name init ->
    (text "let" <+> renderOptions.renderName name <+> text "=") </> renderExpr renderOptions init <> text ";"
  TopImport name ty externalName ->
    text "import"
      <+> renderOptions.renderName name
      <+> text ":"
      <+> renderFuncTy ty
      <+> text "from"
      <+> text externalName

curlies :: forall a. Doc a -> Doc a
curlies = flexGroup <<< encloseEmptyAlt open close (text "{}") <<< indent
  where
  open = text "{" <> break
  close = break <> text "}"

brackets :: forall a. Doc a -> Doc a
brackets = flexGroup <<< encloseEmptyAlt open close (text "[]") <<< indent
  where
  open = text "[" <> D.softBreak
  close = D.softBreak <> text "]"

parensIndent :: forall a. Doc a -> Doc a
parensIndent = flexGroup <<< encloseEmptyAlt open close (text "()") <<< indent
  where
  open = text "(" <> D.softBreak
  close = D.softBreak <> text ")"

parens :: forall a. Doc a -> Doc a
parens = encloseEmptyAlt (text "(") (text ")") (text "()")
