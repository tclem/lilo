module Typed.Checker where

import Typed.Syntax
import Data.Monoid
import Data.Maybe
import qualified Data.List as L
-- import qualified Data.Map as Map

-- well typed program's don't go wrong

data Value = Literal Lit
           | Closure Name Type Expr Scope
  deriving (Eq, Show)

type Scope = [(Name, Type)]

type Result = Either String

check :: Expr -> Result Type
check = go []
  where
    go :: Scope -> Expr -> Result Type
    go context expr = case expr of
      Var n -> maybe (Left ("free variable " <> n)) Right (L.lookup n context)
      Lit l -> Right $ case l of
        LInt _ -> TInt
        LBool _ -> TBool
      App fn arg -> do
        fnTy <- go context fn
        argTy <- go context arg
        case fnTy of
          TArr inTy outTy | inTy == argTy -> Right outTy
                          | otherwise -> Left $ "expected type " <> show inTy <> " but got " <> show argTy
          _ -> Left "expected a function type"
      Lam n ty body -> fmap (TArr ty) (go ((n, ty) : context) body)

-- Γ, _n_ : t |- Var n : t
-- Γ |- \ n : t -> e
-- -----------------
-- Γ, n : t |- e
