{-# LANGUAGE DeriveFunctor, TypeOperators, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

-- Experimenting with Data types à la carte
-- http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf
--
-- Example usage:
-- λ let x :: ALaCarte.Expr (Union '[Val, Add, Mul]) = val 80 ⓧ  val 5 ⊕ val 3
-- λ ALaCarte.eval x
-- 403
-- λ ALaCarte.pretty x
-- "((80 * 5) + 3)"
module ALaCarte where

import Data.Monoid
import Data.Proxy (Proxy(..))
import Data.Union

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

-- Render and Eval instances for Data.Union
instance Apply Render fs => Render (Union fs) where
  render = apply (Proxy :: Proxy Render) render

instance (Apply Eval fs, Apply Functor fs) => Eval (Union fs) where
  evalAlgebra = apply (Proxy :: Proxy Eval) evalAlgebra

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
inject :: (g :< f) => g (Expr (Union f)) -> Expr (Union f)
inject = In . inj

val :: (Val :< f) => Int -> Expr (Union f)
val = inject . Val

infixl 6 ⊕
(⊕) :: (Add :< f) => Expr (Union f) -> Expr (Union f) -> Expr (Union f)
x ⊕ y = inject (Add x y)

infixl 7 ⓧ
(ⓧ) :: (Mul :< f) => Expr (Union f) -> Expr (Union f) -> Expr (Union f)
x ⓧ y = inject (Mul x y)
