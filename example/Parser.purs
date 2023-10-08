module Parser (parseFuncs, parseExpr) where

import Prelude

import AST (Decl(..), Expr(..), Func(..), Op(..))
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array as Array
import Data.Either (Either)
import Data.Identity (Identity)
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Expr (Assoc(..), Operator(..), buildExprParser)
import Text.Parsing.Parser.Language (javaStyle)
import Text.Parsing.Parser.String as PS
import Text.Parsing.Parser.Token as T

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
    [ [ Infix (l.reservedOp "/" $> BinOp Div) AssocRight
      , Infix (l.reservedOp "*" $> BinOp Mul) AssocRight
      ]
    , [ Infix (l.reservedOp "-" $> BinOp Sub) AssocRight
      , Infix (l.reservedOp "+" $> BinOp Add) AssocRight
      ]
    , [ Infix (l.reservedOp "<" $> BinOp Lt) AssocRight
      , Infix (l.reservedOp ">" $> BinOp Gt) AssocRight
      ]
    , [ Infix (l.reservedOp "<=" $> BinOp Lte) AssocRight
      , Infix (l.reservedOp ">=" $> BinOp Gte) AssocRight
      , Infix (l.reservedOp "==" $> BinOp Eq) AssocRight
      ]
    ]
    (atom e)

atom :: Parser (Expr String) -> Parser (Expr String)
atom e =
  map IntLit l.integer
    <|> l.reserved "true" $> BoolLit true
    <|> l.reserved "false" $> BoolLit true
    <|> If <$> (l.reserved "if" *> e) <*> l.braces e <*> (l.reserved "else" *> l.braces e)
    <|>
      (l.identifier >>= \ident -> call e ident <|> pure (Var ident))

call :: Parser (Expr String) -> String -> Parser (Expr String)
call e name =
  Call name <$> l.parens (map Array.fromFoldable (l.commaSep e))

parseExpr :: String -> Either P.ParseError (Expr String)
parseExpr i = P.runParser i (l.whiteSpace *> expr <* PS.eof)

decl :: Parser (Decl String)
decl =
  LetD <$> (l.reserved "let" *> l.identifier <* l.symbol "=") <*> expr <|>
    ExprD <$> expr

func :: Parser (Func String)
func = ado
  l.reserved "fn"
  name <- l.identifier
  params <- l.parens (Array.fromFoldable <$> (l.commaSep l.identifier))
  body <- l.braces (Array.fromFoldable <$> (l.semiSep decl))
  in Func name params body

parseFuncs :: String -> Either P.ParseError (Array (Func String))
parseFuncs i = P.runParser i (l.whiteSpace *> Array.some func <* PS.eof)
