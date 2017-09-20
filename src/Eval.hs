{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}

module Eval where

import Data.Monoid
import Data.Maybe
import Data.Proxy (Proxy(..))
import Data.Union
import qualified Data.List as L

import ALaCarte


data Value = LInt Int
           | LBool Bool
            --  | Closure Name (Expr f) (Scope f)
  deriving (Eq, Show)

-- instance Show (Value f) where
--    showsPrec _ (LInt x) = shows x
--    showsPrec _ (LBool x) = shows x
--    showsPrec _ _ = shows "don't know how to show"

type Name = String
type Scope = [(Name, Value)]

class Functor f => Eval f where
  evalAlgebra :: Scope -> f Value -> Value

instance (Apply Eval fs, Apply Functor fs) => Eval (Union fs) where
  evalAlgebra env = apply (Proxy :: Proxy Eval) (evalAlgebra env)

-- Lookup a Name in an environment.
lookupEnv :: Scope -> Name -> Value
lookupEnv env k = fromMaybe (error ("free variable " <> k)) (L.lookup k env)

-- Evaluate an Expr to a Value
eval :: Eval f => Expr f -> Value
eval = foldExpr (evalAlgebra [])
  where
    foldExpr :: Functor f => (f a -> a) -> Expr f -> a
    foldExpr f (In t) = f (fmap (foldExpr f) t)



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
--
--
-- -- Help sort out how to eval case.
-- -- case' :: Either l r -> (l -> a) -> (r -> a) -> a
-- -- case' e fL fR = case e of
-- --   Left l -> fL l
-- --   Right r -> fR r
