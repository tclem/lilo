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
  eval :: Eval g => Scope' g -> f (Expr g) -> Result (ValueF (Expr g))

instance (Apply Eval fs, Apply Functor fs) => Eval (Union fs) where
  eval env = apply (Proxy :: Proxy Eval) (eval env)

-- Lookup a Name in an environment.
lookupEnv :: Name -> Scope' f -> Result (ValueF (Expr f))
lookupEnv n env = maybe (Left ("free variable " <> n)) Right (L.lookup n env)

-- Extend an environment.
extendEnv :: Name -> ValueF (Expr f) -> Scope' f -> Scope' f
extendEnv n v = (:) (n, v)

-- Evalute
evalExpr :: Eval f => Expr f -> Result (ValueF (Expr f))
evalExpr (In t) = eval [] t


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
