module Parser
  ( parseExpr
  , parseProgram
  ) where

import Prelude

import Ast (Decl(..), Expr(..), FuncTy(..), Lit(..), Op(..), Program, Toplevel(..), ValTy(..))
import Control.Alt ((<|>))
import Control.Lazy (fix)
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

expr :: Parser (Expr String)
expr = fix \e ->
  buildExprParser
    [ [ Infix (l.reservedOp "/" $> BinOpE Div) AssocRight
      , Infix (l.reservedOp "*" $> BinOpE Mul) AssocRight
      ]
    , [ Infix (l.reservedOp "-" $> BinOpE Sub) AssocRight
      , Infix (l.reservedOp "+" $> BinOpE Add) AssocRight
      ]
    , [ Infix (l.reservedOp "<" $> BinOpE Lt) AssocRight
      , Infix (l.reservedOp ">" $> BinOpE Gt) AssocRight
      ]
    , [ Infix (l.reservedOp "<=" $> BinOpE Lte) AssocRight
      , Infix (l.reservedOp ">=" $> BinOpE Gte) AssocRight
      , Infix (l.reservedOp "==" $> BinOpE Eq) AssocRight
      ]
    ]
    (expr1 e)

intLit :: Parser Int
intLit = do
  digits <- many1 T.digit
  l.whiteSpace
  let (s :: String) = CU.fromCharArray (NEA.toArray digits)
  case Int.fromString s of
    Nothing -> P.fail "Failed to parse number"
    Just n -> pure n

expr1 :: Parser (Expr String) -> Parser (Expr String)
expr1 e = block e <|> atom e

varOrCall :: Parser (Expr String) -> Parser (Expr String)
varOrCall e = do
  ident <- l.identifier
  C.optionMaybe (l.parens (l.commaSep e)) >>= case _ of
    Nothing -> pure (VarE ident)
    Just args -> pure (CallE ident (Array.fromFoldable args))

atom :: Parser (Expr String) -> Parser (Expr String)
atom e =
  map LitE lit
    <|> varOrCall e
    <|> IfE <$> (l.reserved "if" *> e) <*> block e <*> (l.reserved "else" *> block e)
    <|> l.parens e

block :: Parser (Expr String) -> Parser (Expr String)
block e =
  BlockE <$> l.braces (Array.fromFoldable <$> l.semiSep (decl e))

lit :: Parser Lit
lit = map IntLit intLit
  <|> l.reserved "true" $> BoolLit true
  <|> l.reserved "false" $> BoolLit true

parseExpr :: String -> Either P.ParseError (Expr String)
parseExpr i = P.runParser i (l.whiteSpace *> expr <* PS.eof)

decl :: Parser (Expr String) -> Parser (Decl String)
decl e =
  LetD <$> (l.reserved "let" *> l.identifier <* l.symbol "=") <*> e
    <|> SetD <$> (l.reserved "set" *> l.identifier <* l.symbol "=") <*> e
    <|>
      ExprD <$> e

valTy :: Parser ValTy
valTy = l.reserved "i32" $> TyI32 <|> l.reserved "bool" $> TyBool

funcTy :: Parser FuncTy
funcTy = ado
  arguments <- l.parens (l.commaSep valTy)
  l.symbol "->"
  result <- (l.reserved "()" $> TyUnit) <|> valTy
  in FuncTy (Array.fromFoldable arguments) result

topFunc :: Parser (Toplevel String)
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

topLet :: Parser (Toplevel String)
topLet = ado
  l.reserved "let"
  n <- l.identifier
  _ <- l.symbol "="
  initializer <- expr
  l.symbol ";"
  in TopLet n initializer

topImport :: Parser (Toplevel String)
topImport = ado
  l.reserved "import"
  name <- l.identifier
  l.symbol ":"
  ty <- funcTy
  l.reserved "from"
  externalName <- l.identifier
  in TopImport name ty externalName

topLevel :: Parser (Toplevel String)
topLevel = topImport <|> topLet <|> topFunc

parseProgram :: String -> Either P.ParseError (Program String)
parseProgram i = P.runParser i (l.whiteSpace *> Array.some topLevel <* PS.eof)
