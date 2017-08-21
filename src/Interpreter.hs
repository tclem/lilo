module Interpreter where

import Syntax
import Data.Monoid
import Data.Maybe
import qualified Data.List as L

data Value = Literal Lit
           | VPair Value Value
           | Closure Name (Elab Type) Scope
  deriving (Eq, Show)

type Scope = [(Name, Value)]

eval :: Elab Type -> Value
eval = go []
  where
    go :: Scope -> Elab Type -> Value
    go env expr = case eout expr of
      Var n -> fromJust (L.lookup n env)
      Lit l -> Literal l
      Fst (EIn (Pair a _) _) -> go env a
      Snd (EIn (Pair _ b) _) -> go env b
      Pair a b -> VPair (go env a) (go env b)
      App a b -> let Closure n body env' = go env a
                 in go ((n, go env b) : env') body
      Lam n _ body -> Closure n body env
