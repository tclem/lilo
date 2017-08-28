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

type TName = String

data Type
  = TVar TName
  | TInt
  | TBool
  | TSum Type Type
  | TPair Type Type
  | TArr Type Type
  deriving(Eq, Ord, Show)

-- Model polymorphic types
data Scheme = Forall [Name] Type
  deriving(Eq, Show)
