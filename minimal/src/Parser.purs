module Parser
  ( parseExpr
  , parseProgram
  ) where

import Prelude

import Ast (Decl(..), Expr(..), Func(..), FuncTy(..), Lit(..), Op(..), Program, Toplevel(..), ValTy(..))
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
expr1 e = block e <|> expr2 e

expr2 :: Parser (Expr String) -> Parser (Expr String)
expr2 e = do
  atoms <- many1 (atom e)
  pure (Array.foldl CallE (NEA.head atoms) (NEA.tail atoms))

atom :: Parser (Expr String) -> Parser (Expr String)
atom e =
  map LitE lit
    <|> map VarE l.identifier
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
valTy = l.reserved "i32" $> I32

funcTy :: Parser FuncTy
funcTy = ado
  arguments <- l.parens (l.commaSep valTy)
  l.symbol "->"
  result <- valTy
  in FuncTy (Array.fromFoldable arguments) result

topFunc :: Parser (Toplevel String)
topFunc = ado
  name <- l.identifier
  params <- many1 l.identifier
  _ <- l.symbol "="
  body <- expr
  in TopFunc (Func name params body)

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
