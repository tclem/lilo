{-# LANGUAGE DeriveFunctor #-}
module Typed2.Syntax where

type Result = Either String

type Name = String

data ExprF a
  = Var Name
  | Lit Lit
  | App a a
  | Lam Name Type a
  deriving (Eq, Functor, Show)

newtype Expr = In { out :: ExprF Expr }
 deriving (Eq, Show)

data Elab a = EIn { eout :: ExprF (Elab a), eann :: a }
 deriving (Eq, Show)

cata :: (ExprF a -> a) -> Expr -> a
cata algebra = go where go = algebra . fmap go . out

data Lit
  = LInt Int
  | LBool Bool
  deriving (Eq, Show)

data Type
  = TInt
  | TBool
  | TArr Type Type
  deriving(Eq, Show)
