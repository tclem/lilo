module Parser where

import Syntax

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (haskellStyle)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where ops = ["\\", "->", ".", ":"]
        names = []
        style = haskellStyle { Tok.reservedOpNames = ops
                             , Tok.reservedNames = names
                             , Tok.commentLine = "#"
                             }

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

contents :: Parser a -> Parser a
contents p = do
  r <- Tok.whiteSpace lexer >> p
  eof
  pure r

parens :: Parser a -> Parser a
parens = Tok.parens lexer

type' :: Parser Type
type' = Ex.buildExpressionParser tyOps (tyLit <|> parens type')
  where
    infixOp x f = Ex.Infix (reservedOp x >> pure f)
    tyOps = [
        [infixOp "->" TArr Ex.AssocRight]
      ]

tyLit :: Parser Type
tyLit = (reservedOp "Bool" >> pure TBool)
    <|> (reservedOp "Int" >> pure TInt)

identifier :: Parser String
identifier = Tok.identifier lexer

bool :: Parser Expr
bool =  (Tok.reserved lexer "true" >> pure (In (Lit (LBool True))))
    <|> (Tok.reserved lexer "false" >> pure (In (Lit (LBool False))))

number :: Parser Expr
number = Tok.natural lexer >>= pure . In . Lit . LInt . fromIntegral

literal :: Parser Expr
literal = number <|> bool

variable :: Parser Expr
variable = identifier >>= pure . In . Var

lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  n <- identifier
  reservedOp ":"
  t <- type'
  reservedOp "."
  body <- expression
  pure (In (Lam n t body))

expression :: Parser Expr
expression = do
  es <- many1 term
  pure (foldl1 ((In .) . App) es)

term :: Parser Expr
term = parens expression <|> literal <|> variable <|> lambda

parseExpr' :: String -> Either ParseError Expr
parseExpr' = parse (contents expression) ""

parseExpr :: String -> Expr
parseExpr = either (error . show) id . parseExpr'
