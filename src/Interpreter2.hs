module Interpreter2 where

import Syntax
import Data.Monoid
import Data.Maybe
import qualified Data.List as L
-- import qualified Data.Map as Map

data Value = Literal Lit
           | Closure Name Expr Scope
  deriving (Eq, Show)

type Scope = [(Name, Value)]

eval :: Expr -> Value
eval = go []
  where
    go :: Scope -> Expr -> Value
    go env expr = case expr of
      Var n -> fromMaybe (error ("free variable " <> n)) (L.lookup n env)
      Lit l -> Literal l
      App a b -> case go env a of
        Closure n body env' -> go ((n, go env b) : env') body
        _ -> error "expected a closure"
      Lam n body -> Closure n body env
