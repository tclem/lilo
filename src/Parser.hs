module Parser where

import Syntax

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (haskellStyle)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where ops = ["\\", "->", "."]
        names = []
        style = haskellStyle { Tok.reservedOpNames = ops
                             , Tok.reservedNames = names
                             , Tok.commentLine = "#"
                             }

contents :: Parser a -> Parser a
contents p = do
  r <- Tok.whiteSpace lexer >> p
  eof
  pure r

parens :: Parser a -> Parser a
parens = Tok.parens lexer

identifier :: Parser String
identifier = Tok.identifier lexer

bool :: Parser Expr
bool =  (Tok.reserved lexer "true" >> pure (Lit (LBool True)))
    <|> (Tok.reserved lexer "false" >> pure (Lit (LBool False)))

number :: Parser Expr
number = Tok.natural lexer >>= pure . Lit . LInt . fromIntegral

literal :: Parser Expr
literal = number <|> bool

variable :: Parser Expr
variable = identifier >>= pure . Var

lambda :: Parser Expr
lambda = do
  Tok.reservedOp lexer "\\"
  args <- many1 identifier
  Tok.reservedOp lexer "->"
  body <- expression
  pure (foldr Lam body args)

expression :: Parser Expr
expression = do
  es <- many1 term
  pure (foldl1 App es)

term :: Parser Expr
term = parens expression <|> literal <|> variable <|> lambda

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents expression) ""
