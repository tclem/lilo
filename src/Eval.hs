{-# LANGUAGE DeriveFunctor, FlexibleContexts, UndecidableInstances #-}

module Eval where

import Data.Monoid
import Data.Maybe
import Data.Proxy (Proxy(..))
import Data.Union
import Control.Monad.Free
import qualified Data.List as L

import ALaCarte

data ValueF f = LInt Int
              | LBool Bool
              | Closure Name f (Scope (ValueF f))
  deriving(Eq, Show, Functor)

type Name = String
type Scope f = [(Name, f)]

type Scope' f = Scope (ValueF (Expr f))

-- Evaluation of Exprs
class Functor f => Eval f where
  evalAlgebra :: Scope' g -> (Scope' g -> Expr g -> ValueF (Expr g)) -> f (Expr g) -> ValueF (Expr g)

instance (Apply Eval fs, Apply Functor fs) => Eval (Union fs) where
  evalAlgebra env c = apply (Proxy :: Proxy Eval) (evalAlgebra env c)

-- Lookup a Name in an environment.
lookupEnv :: Scope' f -> Name -> ValueF (Expr f)
lookupEnv env k = fromMaybe (error ("free variable " <> k)) (L.lookup k env)

-- Evalute
eval :: Eval f => Expr f -> ValueF (Expr f)
eval = eval' []
  where
    eval' :: Eval f => Scope' f -> Expr f -> ValueF (Expr f)
    eval' env (In t) = evalAlgebra env eval' t


-- import Data.Monoid
-- import Data.Maybe
-- import qualified Data.List as L

-- data Value = Literal Lit
--            | VPair Value Value
--            | VLeft Value
--            | VRight Value
--            | Closure Name (Elab Type) Scope
--   deriving (Eq, Show)
--
-- type Scope = [(Name, Value)]
--
-- eval :: Elab Type -> Value
-- eval = eval' []
--   where
--     eval' :: Scope -> Elab Type -> Value
--     eval' env expr = case eout expr of
--       Var n -> fromJust (L.lookup n env)
--       Lit l -> Literal l
--       Pair a b -> VPair (eval' env a) (eval' env b)
--       Fst p -> let VPair a _ = eval' env p in a
--       Snd p -> let VPair _ b = eval' env p in b
--       InL a _ -> VLeft (eval' env a)
--       InR a _ -> VRight (eval' env a)
--       Case e ifL ifR -> case eval' env e of
--         VLeft arg  -> let Closure n body env' = eval' env ifL
--                       in eval' ((n, arg) : env') body
--         VRight arg -> let Closure n body env' = eval' env ifR
--                       in eval' ((n, arg) : env') body
--       App a b -> let Closure n body env' = eval' env a
--                  in eval' ((n, eval' env b) : env') body
--       Lam n _ body -> Closure n body env
