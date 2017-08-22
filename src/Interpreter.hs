module Interpreter where

import Syntax
import Data.Monoid
import Data.Maybe
import qualified Data.List as L

data Value = Literal Lit
           | VPair Value Value
           | VLeft Value
           | VRight Value
           | Closure Name (Elab Type) Scope
  deriving (Eq, Show)

type Scope = [(Name, Value)]

eval :: Elab Type -> Value
eval = eval' []
  where
    eval' :: Scope -> Elab Type -> Value
    eval' env expr = case eout expr of
      Var n -> fromJust (L.lookup n env)
      Lit l -> Literal l
      Pair a b -> VPair (eval' env a) (eval' env b)
      Fst p -> let VPair a _ = eval' env p in a
      Snd p -> let VPair _ b = eval' env p in b
      InL a _ -> VLeft (eval' env a)
      InR a _ -> VRight (eval' env a)
      Case e ifL ifR -> case eval' env e of
        VLeft arg  -> let Closure n body env' = eval' env ifL
                      in eval' ((n, arg) : env') body
        VRight arg -> let Closure n body env' = eval' env ifR
                      in eval' ((n, arg) : env') body
      App a b -> let Closure n body env' = eval' env a
                 in eval' ((n, eval' env b) : env') body
      Lam n _ body -> Closure n body env


-- Help sort out how to eval case.
-- case' :: Either l r -> (l -> a) -> (r -> a) -> a
-- case' e fL fR = case e of
--   Left l -> fL l
--   Right r -> fR r
