{-# LANGUAGE DeriveFunctor #-}

module Syntax where

type Result = Either String

type Name = String

data ExprF a
  = Var Name
  | Lit Lit
  | Pair a a
  | Fst a
  | Snd a
  | InL a Type
  | InR a Type
  | Case a a a
  | App a a
  | Lam Name Type a
  deriving (Eq, Functor, Show)

newtype Expr = In { out :: ExprF Expr }
 deriving (Eq, Show)

data Elab a = EIn { eout :: ExprF (Elab a), eann :: a }
 deriving (Eq, Show)

data Lit
  = LInt Int
  | LBool Bool
  deriving (Eq, Show)

data Type
  = TInt
  | TBool
  | TSum Type Type
  | TPair Type Type
  | TArr Type Type
  deriving(Eq, Show)

-- \ x : (Int + Bool) . case x of (\ l : Int . false) (\ r : Bool . 1)
--
-- \ x : Bool . if x then 1 else true
-- thenT <- check then'
-- elseT <- check else'
-- return (if thenT == elseT then thenT else TSum thenT elseT)
--
-- \ x : Bool . if x then left 1 else right true
--
--
-- \ x : Bool . (if x then left 1 else left 2 : (Int + Bool))
-- \ x : Bool . (if x then (left 1 : (Int + Bool)) else (left 2 : (Int + Bool)))

-- data Zero
-- data One = One
-- data Sum a b = L a | R b
-- type OnePlusOne = Sum One One
-- data Product a b = P a b
-- type OneByOne = Product One One
--
-- type TwobyTwo = Product OnePlusOne OnePlusOne
-- P (L One) (L One)
-- P (L One) (R One)
-- P (R One) (R One)
-- P (R One) (L One)
--
-- type Exponential a b = a -> b

cata :: (ExprF a -> a) -> Expr -> a
cata algebra = go where go = algebra . fmap go . out
