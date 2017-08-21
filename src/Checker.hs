module Checker where

import Syntax
import Data.Monoid
import qualified Data.List as L

-- well typed program's don't go wrong

data Value = Literal Lit
           | Closure Name Type Expr Scope
  deriving (Eq, Show)

type Scope = [(Name, Type)]

check :: Expr -> Result (Elab Type)
check = go []
  where
    go :: Scope -> Expr -> Result (Elab Type)
    go context expr = case out expr of
      Var n -> maybe (Left ("free variable " <> n)) (Right . EIn (Var n)) (L.lookup n context)
      Lit l -> Right $ EIn (Lit l) (case l of
        LInt _ -> TInt
        LBool _ -> TBool)
      Pair a b -> do
        f@(EIn _ fstTy) <- go context a
        s@(EIn _ sndTy) <- go context b
        pure (EIn (Pair f s) (TPair fstTy sndTy))
      Fst a -> do
        p@(EIn tm ty) <- go context a
        case ty of
          TPair ty1 _ -> pure (EIn (Fst p) ty1)
          _ -> Left $ "fst expected pair but got " <> show ty
      Snd a -> do
        p@(EIn tm ty) <- go context a
        case ty of
          TPair _ ty2 -> pure (EIn (Snd p) ty2)
          _ -> Left $ "snd expected pair but got " <> show ty
      App fn arg -> do
        fnE@(EIn fnTm fnTy) <- go context fn
        argE@(EIn argTm argTy) <- go context arg
        case fnTy of
          TArr inTy outTy | inTy == argTy -> Right (EIn (App fnE argE) outTy)
                          | otherwise -> Left $ "expected type " <> show inTy <> " but got " <> show argTy
          _ -> Left "expected a function type"
      Lam n ty body -> do
        bodyE@(EIn _ retTy) <- go ((n, ty) : context) body
        pure (EIn (Lam n ty bodyE) (TArr ty retTy))

-- Implement in Syntax:
  -- pairs
  -- fst/snd
  -- -> sums

-- Then, think about, study:
-- type inference
-- unification, hindley-milner (algorithm m, algorithm w)

-- Γ, _n_ : t |- Var n : t
-- Γ |- \ n : t -> e
-- -----------------
-- Γ, n : t |- e
