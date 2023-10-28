module Parser
  ( parseExpr
  , parseProgram
  ) where

import Prelude

import Ast (Decl(..), Expr, Expr'(..), FuncTy(..), Intrinsic(..), Lit(..), Op(..), Program, SetTarget(..), Toplevel(..), Ty(..))
import Control.Alt ((<|>))
import Control.Lazy (defer)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Either (Either)
import Data.Identity (Identity)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as CU
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Unsafe (unsafeRegex)
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
      { reservedNames = [ "fn", "true", "false", "if", "else", "let", "set", "while" ]
      , reservedOpNames = [ "+", "-", "*", "/", "<", ">", "<=", ">=", "==", "!=" ]
      }

identStartR :: Regex
identStartR = unsafeRegex "[a-z_]" mempty

identContinueR :: Regex
identContinueR = unsafeRegex "[a-zA-Z0-9_]" mempty

upperIdentStartR :: Regex
upperIdentStartR = unsafeRegex "[A-Z]" mempty

identStart :: Parser Char
identStart =
  PS.satisfy (Regex.test identStartR <<< CU.singleton)

identContinue :: Parser Char
identContinue =
  PS.satisfy (Regex.test identContinueR <<< CU.singleton)

lowerIdent :: Parser String
lowerIdent = l.lexeme do
  c <- identStart
  cs <- Array.many identContinue
  pure (CU.singleton c <> CU.fromCharArray cs)

upperIdent :: Parser String
upperIdent = l.lexeme do
  c <- PS.satisfy (Regex.test upperIdentStartR <<< CU.singleton)
  cs <- Array.many identContinue
  pure (CU.singleton c <> CU.fromCharArray cs)

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
      , Infix (l.reservedOp "!=" $> \l' r' -> noNote (BinOpE Neq l' r')) AssocRight
      ]
    , [ Infix (l.reservedOp "&&" $> \l' r' -> noNote (BinOpE And l' r')) AssocRight
      , Infix (l.reservedOp "||" $> \l' r' -> noNote (BinOpE Or l' r')) AssocRight
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

exprIdx :: Expr Unit String -> Parser (Expr Unit String)
exprIdx e = C.optionMaybe (l.brackets expr) >>= case _ of
  Nothing -> C.optionMaybe (l.symbol "." *> lowerIdent) >>= case _ of
    Just fieldIx -> exprIdx (noNote (StructIdxE e fieldIx))
    Nothing -> pure e
  Just ix -> exprIdx (noNote (ArrayIdxE e ix))

expr2 :: Parser (Expr Unit String)
expr2 = defer \_ -> do
  e <- atom
  exprIdx e

varOrCall :: Parser (Expr Unit String)
varOrCall = defer \_ -> do
  ident <- lowerIdent
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

structExpr :: Parser (Expr Unit String)
structExpr = defer \_ -> do
  name <- upperIdent
  fields <- l.braces (l.commaSep field)
  pure (noNote (StructE name (Array.fromFoldable fields)))
  where
  field = do
    name <- lowerIdent
    _ <- l.symbol "="
    e <- expr
    pure { name, expr: e }

atom :: Parser (Expr Unit String)
atom = defer \_ ->
  map (noNote <<< LitE) lit
    <|> ifExpr
    <|> l.parens expr
    <|> map (noNote <<< ArrayE) (l.brackets (Array.fromFoldable <$> l.commaSep expr))
    <|> structExpr
    <|> intrinsic
    <|> varOrCall

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
  <|> l.reserved "false" $> BoolLit false

parseExpr :: String -> Either P.ParseError (Expr Unit String)
parseExpr i = P.runParser i (l.whiteSpace *> expr <* PS.eof)

decl :: Parser (Decl Unit String)
decl = defer \_ ->
  LetD <$> (l.reserved "let" *> lowerIdent <* l.symbol "=") <*> expr
    <|> SetD <$> (l.reserved "set" *> setTarget <* l.symbol "=") <*> expr
    <|> WhileD <$> (l.reserved "while" *> expr) <*> block
    <|> ExprD <$> expr

setTarget :: Parser (SetTarget Unit String)
setTarget = do
  name <- lowerIdent
  C.optionMaybe (l.brackets expr) >>= case _ of
    Nothing -> pure (VarST name)
    Just ix -> pure (ArrayIdxST name ix)

valTy :: Parser (Ty String)
valTy = defer \_ ->
  l.reserved "i32" $> TyI32
    <|> l.reserved "f32" $> TyF32
    <|> l.reserved "bool" $> TyBool
    <|> map TyArray (l.brackets valTy)
    <|> map TyCons upperIdent

funcTy :: Parser (FuncTy String)
funcTy = ado
  arguments <- l.parens (l.commaSep valTy)
  l.symbol "->"
  result <- (l.reserved "()" $> TyUnit) <|> valTy
  in FuncTy (Array.fromFoldable arguments) result

topFunc :: Parser (Toplevel Unit String)
topFunc = ado
  l.reserved "fn"
  name <- lowerIdent
  params <- l.parens (l.commaSep param)
  returnTy <- C.option TyUnit (l.symbol ":" *> valTy)
  _ <- l.symbol "="
  body <- expr
  in TopFunc { name, export: Just name, params: Array.fromFoldable params, returnTy, body }

param :: Parser { name :: String, ty :: Ty String }
param = ado
  name <- lowerIdent
  _ <- l.symbol ":"
  ty <- valTy
  in { name, ty }

topLet :: Parser (Toplevel Unit String)
topLet = ado
  l.reserved "let"
  n <- lowerIdent
  _ <- l.symbol "="
  initializer <- expr
  l.symbol ";"
  in TopLet n initializer

topImport :: Parser (Toplevel Unit String)
topImport = ado
  l.reserved "import"
  name <- lowerIdent
  l.symbol ":"
  ty <- funcTy
  l.reserved "from"
  externalName <- lowerIdent
  in TopImport name ty externalName

topStruct :: Parser (Toplevel Unit String)
topStruct = ado
  l.reserved "struct"
  name <- upperIdent
  fields <- l.braces (l.commaSep param)
  in TopStruct name (Array.fromFoldable fields)

topLevel :: Parser (Toplevel Unit String)
topLevel = topImport <|> topLet <|> topFunc <|> topStruct

parseProgram :: String -> Either P.ParseError (Program Unit String)
parseProgram i = P.runParser i (l.whiteSpace *> Array.some topLevel <* PS.eof)
