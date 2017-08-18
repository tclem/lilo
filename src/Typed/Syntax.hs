module Typed.Syntax where

type Name = String

data Expr
  = Var Name
  | Lit Lit
  | App Expr Expr
  | Lam Name Type Expr
  deriving (Eq, Show)

data Lit
  = LInt Int
  | LBool Bool
  deriving (Eq, Show)

data Type
  = TInt
  | TBool
  | TArr Type Type
  deriving(Eq, Show)
