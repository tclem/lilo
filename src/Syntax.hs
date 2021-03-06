{-# LANGUAGE OverloadedStrings, DeriveFunctor, TypeOperators, FlexibleContexts, DeriveGeneric, DataKinds #-}

module Syntax where

import Data.Union
import Text.PrettyPrint as PP hiding (render)
import Control.Monad.Free
import Data.Functor.Classes
import Data.Functor.Classes.Show.Generic
import GHC.Generics

import ALaCarte
import Pretty
import Eval


type Syntax = Expr (Union '[Syntax.Integer, Boolean, Variable, Lambda, Application])

-- Booleans

newtype Boolean a = Boolean Bool deriving (Functor, Show, Generic1)

instance Show1 Boolean where liftShowsPrec = genericLiftShowsPrec
instance Render Boolean where render _ (Boolean x) = text (show x)

instance Eval Boolean where
  eval _ (Boolean x) = Right (LBool x)

bool :: (Boolean :< f) => Bool -> Expr (Union f)
bool = inject . Boolean

true :: (Boolean :< f) => Expr (Union f)
true = bool True

false :: (Boolean :< f) => Expr (Union f)
false = bool False


-- Integers

newtype Integer a = Integer Int deriving (Functor, Show, Generic1)

instance Show1 Syntax.Integer where liftShowsPrec = genericLiftShowsPrec
instance Render Syntax.Integer where render _ (Integer x) = text (show x)

instance Eval Syntax.Integer where
  eval _ (Integer x) = Right (LInt x)

int :: (Syntax.Integer :< f) => Int -> Expr (Union f)
int = inject . Integer


-- Variables

newtype Variable a = Variable String deriving (Functor, Show, Generic1)

instance Show1 Variable where liftShowsPrec = genericLiftShowsPrec
instance Render Variable where render _ (Variable x) = text x

instance Eval Variable where
  eval env (Variable x) = lookupEnv x env

var :: (Variable :< f) => String -> Expr (Union f)
var = inject . Variable


-- Lambdas

data Lambda a = Lambda String a deriving (Functor, Show, Generic1)

instance Show1 Lambda where liftShowsPrec = genericLiftShowsPrec
instance Render Lambda where
  render d (Lambda name (In t)) = parensIf (d > 0) $
    char '\\'
    <> text name
    <+> "->"
    <+> render (succ d) t

instance Eval Lambda where
  eval env (Lambda name body) = Right (Closure name body env)

lam :: (Lambda :< f) => String -> Expr (Union f) -> Expr (Union f)
lam n body = inject (Lambda n body)


-- Application

data Application a = Application a a deriving (Functor, Show, Generic1)

instance Show1 Application where liftShowsPrec = genericLiftShowsPrec
instance Render Application where
  render d (Application (In e1) (In e2)) = parensIf (d > 0) $
    render (succ d) e1 <+> render d e2

instance Eval Application where
  eval env (Application (In a) (In b)) =
    let Right (Closure name (In body) env') = eval env a
        Right value = eval env b
    in eval (extendEnv name value env') body

app :: (Application :< f) => Expr (Union f) -> Expr (Union f) -> Expr (Union f)
app a b = inject (Application a b)


-- type Result = Either String
--
-- type Name = String
--
-- data ExprF a
--   = Var Name
--   | Lit Lit
--   | Pair a a
--   | Fst a
--   | Snd a
--   | InL a Type
--   | InR a Type
--   | Case a a a
--   | App a a
--   | Lam Name Type a
--   deriving (Eq, Functor, Show)
--
-- newtype Expr = In { out :: ExprF Expr }
--  deriving (Eq, Show)
--
-- data Elab a = EIn { eout :: ExprF (Elab a), eann :: a }
--  deriving (Eq, Show)
--
-- data Lit
--   = LInt Int
--   | LBool Bool
--   deriving (Eq, Show)
--
-- type TName = String
--
-- data Type
--   = TVar TName
--   | TInt
--   | TBool
--   | TSum Type Type
--   | TPair Type Type
--   | TArr Type Type
--   deriving(Eq, Ord, Show)
--
-- -- Model polymorphic types
-- data Scheme = Forall [Name] Type
--   deriving(Eq, Show)
