module Printer (printFuncs, printProgram, printExpr, printDecl, renderAll, renderNone) where

import Prelude

import Ast (Decl(..), Expr, Expr'(..), Func, FuncTy(..), Lit(..), Op(..), Program, SetTarget(..), Toplevel(..), Ty(..))
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe as Maybe
import Dodo (Doc, break, encloseEmptyAlt, flexGroup, indent, plainText, print, text, twoSpaces, (<+>), (</>))
import Dodo as D
import Rename as Rename

type RenderOptions note name a =
  { renderNote :: note -> Doc a -> Doc a
  , renderName :: name -> Doc a
  }

renderTyNote :: forall a. (Rename.Var -> Doc a) -> Ty Rename.Var -> Doc a -> Doc a
renderTyNote renderName ty doc = parens (doc <+> text ":" <+> renderTy renderName ty)

renderAll :: forall a. Map Rename.Var String -> RenderOptions (Ty Rename.Var) Rename.Var a
renderAll nameMap = do
  let
    renderName = case _ of
      Rename.BuiltinV n -> text n.name
      v -> text (Maybe.maybe "$UNKNOWN" (\n -> show v <> n) (Map.lookup v nameMap))
  { renderNote: renderTyNote renderName
  , renderName
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
  Neq -> "!="
  And -> "&&"
  Or -> "||"

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
  IntrinsicE i args ->
    text (show i) <> parensIndent (D.foldWithSeparator (text "," <> D.spaceBreak) (map (renderExpr renderOptions) args))
  BlockE body ->
    curlies (D.foldWithSeparator (text ";" <> break) (map (renderDecl renderOptions) body))
  ArrayE elements ->
    brackets (D.foldWithSeparator (text "," <> D.spaceBreak) (map (renderExpr renderOptions) elements))
  ArrayIdxE array index ->
    renderExpr renderOptions array <> brackets (renderExpr renderOptions index)
  StructE name fields -> do
    let
      renderField { name: name', expr: expr' } =
        renderOptions.renderName name' <+> text "=" <+> renderExpr renderOptions expr'
      fields' =
        D.foldWithSeparator (text "," <> D.spaceBreak) (map renderField fields)
    renderOptions.renderName name <+> curlies fields'
  StructIdxE struct index ->
    renderExpr renderOptions struct <> text "." <> renderOptions.renderName index

renderDecl :: forall a note name. RenderOptions note name a -> Decl note name -> Doc a
renderDecl renderOptions = case _ of
  LetD name expr ->
    (text "let" <+> renderOptions.renderName name <+> text "=") </> indent (renderExpr renderOptions expr)
  SetD setTarget expr ->
    (text "set" <+> renderSetTarget renderOptions setTarget <+> text "=") </> renderExpr renderOptions expr
  ExprD expr ->
    renderExpr renderOptions expr
  WhileD cond expr ->
    text "while" <+> renderExpr renderOptions cond <+> renderExpr renderOptions expr

renderSetTarget :: forall a note name. RenderOptions note name a -> SetTarget note name -> Doc a
renderSetTarget renderOptions = case _ of
  VarST ty n ->
    renderOptions.renderNote ty (renderOptions.renderName n)
  ArrayIdxST ty n ix ->
    renderOptions.renderNote ty (renderOptions.renderName n) <> brackets (renderExpr renderOptions ix)
  StructIdxST ty n ix ->
    renderOptions.renderNote ty (renderOptions.renderName n) <> text "." <> renderOptions.renderName ix

renderFunc :: forall a note name. RenderOptions note name a -> Func note name -> Doc a
renderFunc renderOptions func = do
  let
    headerD = text "fn" <+> renderOptions.renderName func.name
    paramD =
      parensIndent (D.foldWithSeparator (text "," <> D.spaceBreak) (map renderParam func.params))
    returnTyD = case func.returnTy of
      TyUnit -> D.space <> text ":" <+> renderTy renderOptions.renderName func.returnTy
      _ -> mempty
    bodyD = renderExpr renderOptions func.body
  (headerD <+> paramD <> returnTyD <+> text "=") </> indent bodyD
  where
  renderParam { name, ty } = renderOptions.renderName name <+> text ":" <+> renderTy renderOptions.renderName ty

renderTy :: forall a name. (name -> Doc a) -> Ty name -> Doc a
renderTy renderName = case _ of
  TyI32 -> text "i32"
  TyF32 -> text "f32"
  TyBool -> text "bool"
  TyUnit -> text "()"
  TyArray t -> text "[" <> renderTy renderName t <> text "]"
  TyCons c -> renderName c

renderFuncTy :: forall a note name. RenderOptions note name a -> FuncTy name -> Doc a
renderFuncTy renderOptions = case _ of
  FuncTy arguments result ->
    parens (D.foldWithSeparator (text "," <> D.space) (map (renderTy renderOptions.renderName) arguments))
      <+> text "->"
      <+> renderTy renderOptions.renderName result

renderToplevel :: forall a note name. RenderOptions note name a -> Toplevel note name -> Doc a
renderToplevel renderOptions = case _ of
  TopFunc func -> renderFunc renderOptions func
  TopLet name init ->
    (text "let" <+> renderOptions.renderName name <+> text "=") </> renderExpr renderOptions init <> text ";"
  TopStruct name fields -> do
    let renderField { name: fieldName, ty } = renderOptions.renderName fieldName <+> text ":" <+> renderTy renderOptions.renderName ty
    (text "struct" <+> renderOptions.renderName name)
      </> curlies (D.foldWithSeparator (text "," <> D.spaceBreak) (map renderField fields))
  TopImport name ty externalName ->
    text "import"
      <+> renderOptions.renderName name
      <+> text ":"
      <+> renderFuncTy renderOptions ty
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
