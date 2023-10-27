module Parser
  ( parseExpr
  , parseProgram
  ) where

import Prelude

import Ast (Decl(..), Expr, Expr'(..), FuncTy(..), Intrinsic(..), Lit(..), Op(..), Program, SetTarget(..), Toplevel(..), ValTy(..))
import Control.Alt ((<|>))
import Control.Lazy (defer)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Either (Either)
import Data.Identity (Identity)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as CU
import Parsing as P
import Parsing.Combinators as C
import Parsing.Combinators.Array (many1)
import Parsing.Expr (Assoc(..), Operator(..), buildExprParser)
import Parsing.Language (javaStyle)
import Parsing.String as PS
import Parsing.Token as T

type Parser = P.Parser String

l :: T.GenTokenParser String Identity
l = T.makeTokenParser langStyle
  where
  javaDef = T.unGenLanguageDef javaStyle
  langStyle = T.LanguageDef langDef
  langDef =
    javaDef
      { reservedNames = [ "fn", "true", "false", "if", "else", "let" ]
      , reservedOpNames = [ "+", "-", "*", "/", "<", ">", "<=", ">=", "==" ]
      }

noNote :: Expr' Unit String -> Expr Unit String
noNote e = { expr: e, note: unit }

expr :: Parser (Expr Unit String)
expr = defer \_ ->
  buildExprParser
    [ [ Infix (l.reservedOp "/" $> \l' r' -> noNote (BinOpE Div l' r')) AssocRight
      , Infix (l.reservedOp "*" $> \l' r' -> noNote (BinOpE Mul l' r')) AssocRight
      ]
    , [ Infix (l.reservedOp "-" $> \l' r' -> noNote (BinOpE Sub l' r')) AssocRight
      , Infix (l.reservedOp "+" $> \l' r' -> noNote (BinOpE Add l' r')) AssocRight
      ]
    , [ Infix (l.reservedOp "<" $> \l' r' -> noNote (BinOpE Lt l' r')) AssocRight
      , Infix (l.reservedOp ">" $> \l' r' -> noNote (BinOpE Gt l' r')) AssocRight
      ]
    , [ Infix (l.reservedOp "<=" $> \l' r' -> noNote (BinOpE Lte l' r')) AssocRight
      , Infix (l.reservedOp ">=" $> \l' r' -> noNote (BinOpE Gte l' r')) AssocRight
      , Infix (l.reservedOp "==" $> \l' r' -> noNote (BinOpE Eq l' r')) AssocRight
      ]
    ]
    expr1

intLit :: Parser Int
intLit = do
  digits <- many1 T.digit
  l.whiteSpace
  let (s :: String) = CU.fromCharArray (NEA.toArray digits)
  case Int.fromString s of
    Nothing -> P.fail "Failed to parse number"
    Just n -> pure n

expr1 :: Parser (Expr Unit String)
expr1 = defer \_ -> block <|> expr2

arrayIdx :: Expr Unit String -> Parser (Expr Unit String)
arrayIdx e = C.optionMaybe (l.brackets expr) >>= case _ of
  Nothing -> pure e
  Just ix -> arrayIdx (noNote (ArrayIdxE e ix))

expr2 :: Parser (Expr Unit String)
expr2 = defer \_ -> do
  e <- atom
  arrayIdx e

varOrCall :: Parser (Expr Unit String)
varOrCall = defer \_ -> do
  ident <- l.identifier
  C.optionMaybe (l.parens (l.commaSep expr)) >>= case _ of
    Nothing -> pure (noNote (VarE ident))
    Just args -> pure (noNote (CallE ident (Array.fromFoldable args)))

ifExpr :: Parser (Expr Unit String)
ifExpr = defer \_ ->
  map
    noNote
    ( IfE <$> (l.reserved "if" *> expr)
        <*> block
        <*> (l.reserved "else" *> (block <|> ifExpr))
    )

atom :: Parser (Expr Unit String)
atom = defer \_ ->
  map (noNote <<< LitE) lit
    <|> varOrCall
    <|> ifExpr
    <|> l.parens expr
    <|> map (noNote <<< ArrayE) (l.brackets (Array.fromFoldable <$> l.commaSep expr))
    <|> intrinsic

intrinsic :: Parser (Expr Unit String)
intrinsic = do
  _ <- PS.char '@'
  name <- intrinsicName
  args <- l.parens (l.commaSep expr)
  pure (noNote (IntrinsicE name (Array.fromFoldable args)))

intrinsicName :: Parser Intrinsic
intrinsicName =
  l.reserved "array_len" $> ArrayLen
    <|> l.reserved "array_new" $> ArrayNew

block :: Parser (Expr Unit String)
block = defer \_ ->
  noNote <<< BlockE <$> l.braces (Array.fromFoldable <$> l.semiSep decl)

foreign import parseFloat :: String -> Number

numberLit :: Parser Lit
numberLit = do
  i <- intLit
  C.optionMaybe (l.symbol ".") >>= case _ of
    Nothing -> pure (IntLit i)
    Just _ -> do
      digits <- many1 T.digit
      l.whiteSpace
      let s = CU.fromCharArray (NEA.toArray digits)
      pure (FloatLit (parseFloat (show i <> "." <> s)))

lit :: Parser Lit
lit = numberLit
  <|> l.reserved "true" $> BoolLit true
  <|> l.reserved "false" $> BoolLit true

parseExpr :: String -> Either P.ParseError (Expr Unit String)
parseExpr i = P.runParser i (l.whiteSpace *> expr <* PS.eof)

decl :: Parser (Decl Unit String)
decl = defer \_ ->
  LetD <$> (l.reserved "let" *> l.identifier <* l.symbol "=") <*> expr
    <|> SetD <$> (l.reserved "set" *> setTarget <* l.symbol "=") <*> expr
    <|> ExprD <$> expr

setTarget :: Parser (SetTarget Unit String)
setTarget = do
  name <- l.identifier
  C.optionMaybe (l.brackets expr) >>= case _ of
    Nothing -> pure (VarST name)
    Just ix -> pure (ArrayIdxST name ix)

valTy :: Parser ValTy
valTy = defer \_ ->
  l.reserved "i32" $> TyI32
    <|> l.reserved "f32" $> TyF32
    <|> l.reserved "bool" $> TyBool
    <|> map TyArray (l.brackets valTy)

funcTy :: Parser FuncTy
funcTy = ado
  arguments <- l.parens (l.commaSep valTy)
  l.symbol "->"
  result <- (l.reserved "()" $> TyUnit) <|> valTy
  in FuncTy (Array.fromFoldable arguments) result

topFunc :: Parser (Toplevel Unit String)
topFunc = ado
  l.reserved "fn"
  name <- l.identifier
  params <- l.parens (l.commaSep param)
  returnTy <- C.option TyUnit (l.symbol ":" *> valTy)
  _ <- l.symbol "="
  body <- expr
  in TopFunc { name, export: Just name, params: Array.fromFoldable params, returnTy, body }
  where
  param = ado
    name <- l.identifier
    _ <- l.symbol ":"
    ty <- valTy
    in { name, ty }

topLet :: Parser (Toplevel Unit String)
topLet = ado
  l.reserved "let"
  n <- l.identifier
  _ <- l.symbol "="
  initializer <- expr
  l.symbol ";"
  in TopLet n initializer

topImport :: Parser (Toplevel Unit String)
topImport = ado
  l.reserved "import"
  name <- l.identifier
  l.symbol ":"
  ty <- funcTy
  l.reserved "from"
  externalName <- l.identifier
  in TopImport name ty externalName

topLevel :: Parser (Toplevel Unit String)
topLevel = topImport <|> topLet <|> topFunc

parseProgram :: String -> Either P.ParseError (Program Unit String)
parseProgram i = P.runParser i (l.whiteSpace *> Array.some topLevel <* PS.eof)
