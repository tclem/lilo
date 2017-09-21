module Parser where

import Syntax

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (haskellStyle)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where ops = ["\\", "->", ".", ",", ":", "+"]
        names = []
        style = haskellStyle { Tok.reservedOpNames = ops
                             , Tok.reservedNames = names
                             , Tok.commentLine = "#"
                             }

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

contents :: Parser a -> Parser a
contents p = do
  r <- Tok.whiteSpace lexer >> p
  eof
  pure r

parens :: Parser a -> Parser a
parens = Tok.parens lexer

identifier :: Parser String
identifier = Tok.identifier lexer


-- Parse to Syntax Expressions
boolean :: Parser Syntax
boolean =  (reservedOp "true" >> pure true)
    <|> (reservedOp "false" >> pure false)

number :: Parser Syntax
number = Tok.natural lexer >>= pure . int . fromIntegral

literal :: Parser Syntax
literal = number <|> boolean

variable :: Parser Syntax
variable = identifier >>= pure . var

lambda :: Parser Syntax
lambda = do
  reservedOp "\\"
  n <- identifier
  reservedOp "."
  body <- expression
  pure (lam n body)

expression :: Parser Syntax
expression = do
  es <- many1 term
  pure (foldl1 app es)

term :: Parser Syntax
term =  parens expression
    <|> literal
    <|> lambda
    <|> variable

parseExpr :: String -> Syntax
parseExpr = either (error . show) id . parseExpr'
 where
    parseExpr' :: String -> Either ParseError Syntax
    parseExpr' = parse (contents expression) ""


-- type' :: Parser Type
-- type' = Ex.buildExpressionParser tyOps (ty <|> parens type')
--   where
--     infixOp x f = Ex.Infix (reservedOp x >> pure f)
--     tyOps = [
--           [infixOp "->" TArr Ex.AssocRight]
--         , [infixOp "," TPair Ex.AssocRight]
--         , [infixOp "+" TSum Ex.AssocRight]
--       ]
--
-- ty :: Parser Type
-- ty = tyLit
--
-- tyLit :: Parser Type
-- tyLit = (reservedOp "Bool" >> pure TBool)
--     <|> (reservedOp "Int" >> pure TInt)
--
-- pair :: Parser Expr
-- pair = do
--   a <- literal
--   reservedOp ","
--   b <- literal
--   pure (In (Pair a b))
--
-- fst' :: Parser Expr
-- fst' = reservedOp "fst" >> term >>= pure . In . Fst
--
-- snd' :: Parser Expr
-- snd' = reservedOp "snd" >> term >>= pure . In . Snd
--
-- right :: Parser Expr
-- right = do
--   t <- reservedOp "right" >> term
--   reservedOp ":"
--   ty <- type'
--   pure (In (InR t ty))
--
-- left :: Parser Expr
-- left = do
--   t <- reservedOp "left" >> term
--   reservedOp ":"
--   ty <- type'
--   pure (In (InL t ty))
--
-- case' :: Parser Expr
-- case' = do
--   reservedOp "case"
--   e <- term
--   reservedOp "of"
--   l <- term
--   r <- term
--   pure (In (Case e l r))
--
-- term :: Parser Expr
-- term =  parens expression
--     <|> fst'
--     <|> snd'
--     <|> try pair
--     <|> right
--     <|> left
--     <|> case'
--     <|> literal
--     <|> lambda
--     <|> variable
