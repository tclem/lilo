module Interpreter where

import Syntax
import Data.Monoid

data Value = Literal Lit | Lambda Name Expr
  deriving (Eq, Show)

-- church-encoded booleans
-- naturals
-- lists
-- Write the y combinator and omega.
-- Y diverges with call by evaluation order (which is what I have)
-- look at Z instead (call by value Y combination or the strict Y combinator)
-- Small step reduction

-- Big step (do the whole thing recursively) *
-- reducing to a value
-- what's a value? Anything that can't be further reduced. e.g. a Literal.

-- Read: "The structure and intepretation of computer programs"

-- (\x -> x) 1

eval :: Expr -> Value
eval e = case e of
  Var n -> error $ "found var '" <> n <> "'. Var must appear inside a lambda"
  Lit x -> Literal x
  Lam n body -> Lambda n body
  App a b -> case eval a of
    Lambda n body -> eval $ substitute n (quote (eval b)) body
    _ -> error "can only apply lambda"

substitute :: Name -> Expr -> Expr -> Expr
substitute name expr body = case body of
  Var n | n == name -> expr
        | otherwise -> body
  Lit l -> body
  Lam n body' | n == name -> body
              | otherwise -> Lam n (substitute name expr body')
  App a b -> App (substitute name expr a) (substitute name expr b)

quote :: Value -> Expr
quote (Literal l) = Lit l
quote (Lambda name body) = Lam name body
