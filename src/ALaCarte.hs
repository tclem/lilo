{-# LANGUAGE DeriveFunctor, TypeOperators, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

-- Experimenting with Data types à la carte
-- http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf
--
-- Example usage:
-- λ let x :: ALaCarte.Expr (Val :+: Mul :+: Add) = val 80 ⓧ val 5 ⊕ val 3
-- λ ALaCarte.eval x
-- 403
-- λ ALaCarte.pretty x
-- "((80 * 5) + 3)"
module ALaCarte where

import Data.Monoid

-- Expressions parameterized by the signature of the expression constructor.
newtype Expr f = In (f (Expr f))

-- Evaluation

-- Eval type class
class Functor f => Eval f where
  evalAlgebra :: f Int -> Int

eval :: Eval f => Expr f -> Int
eval = foldExpr evalAlgebra

foldExpr :: Functor f => (f a -> a) -> Expr f -> a
foldExpr f (In t) = f (fmap (foldExpr f) t)

-- Pretty printing

class Render f where
  render ::Render g => f (Expr g) -> String

pretty :: Render f => Expr f -> String
pretty (In t) = render t


-- :<: type class defines type constraints so that we can say that a parameter
-- supports a specific data type.
class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a

-- :+: defines the coproduct of two type constructors
infixr 5 :+:
data (f :+: g) e = Inl (f e) | Inr (g e) deriving (Functor)

instance (Eval f, Eval g) => Eval (f :+: g) where
  evalAlgebra (Inl x) = evalAlgebra x
  evalAlgebra (Inr y) = evalAlgebra y
instance (Render f, Render g) => Render (f :+: g) where
  render (Inl x) = render x
  render (Inr y) = render y

instance Functor f => f :<: f where
  inj = id
instance {-# OVERLAPPING #-} (Functor f, Functor g) => f :<: (f :+: g) where
  inj = Inl
instance {-# OVERLAPPING #-} (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = Inr . inj



-- Specific expression data types (these are the à la carte data types).
newtype Val e = Val Int deriving (Functor)
instance Eval Val where
  evalAlgebra (Val x) = x
instance Render Val where
  render (Val x) = show x

data Add e = Add e e deriving (Functor)
instance Eval Add where
  evalAlgebra (Add x y) = x + y
instance Render Add where
  render (Add x y) = "(" <> pretty x <> " + " <> pretty y <> ")"

data Mul x = Mul x x  deriving (Functor)
instance Eval Mul where
  evalAlgebra (Mul x y) = x * y
instance Render Mul where
  render (Mul x y) = "(" <> pretty x <> " * " <> pretty y <> ")"

-- Smart constructors
inject :: (g :<: f) => g (Expr f) -> Expr f
inject = In . inj

val :: (Val :<: f) => Int -> Expr f
val = inject . Val

infixl 6 ⊕
(⊕) :: (Add :<: f) => Expr f -> Expr f -> Expr f
x ⊕ y = inject (Add x y)

infixl 7 ⓧ
(ⓧ) :: (Mul :<: f) => Expr f -> Expr f -> Expr f
x ⓧ y = inject (Mul x y)
