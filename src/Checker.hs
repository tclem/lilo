module Checker where

import Syntax
import Data.Monoid hiding (Sum)
import Control.Monad
import qualified Data.List as L

-- well typed program's don't check' wrong

data Value = Literal Lit
           | Closure Name Type Expr Scope
  deriving (Eq, Show)

type Scope = [(Name, Type)]

check :: Expr -> Result (Elab Type)
check = check' []
  where
    check' :: Scope -> Expr -> Result (Elab Type)
    check' ctx expr = case out expr of
      Var n -> maybe (Left ("free variable " <> n)) (Right . EIn (Var n)) (L.lookup n ctx)
      Lit l -> Right $ EIn (Lit l) (case l of
        LInt _ -> TInt
        LBool _ -> TBool)
      Pair a b -> do
        f@(EIn _ fstTy) <- check' ctx a
        s@(EIn _ sndTy) <- check' ctx b
        pure (EIn (Pair f s) (TPair fstTy sndTy))
      Fst a -> do
        p@(EIn tm ty) <- check' ctx a
        case ty of
          TPair ty1 _ -> pure (EIn (Fst p) ty1)
          _ -> Left $ "fst expected pair but got " <> show ty
      Snd a -> do
        p@(EIn tm ty) <- check' ctx a
        case ty of
          TPair _ ty2 -> pure (EIn (Snd p) ty2)
          _ -> Left $ "snd expected pair but got " <> show ty
      InL l (TSum t1 t2) -> do
        lE@(EIn tm ty) <- check' ctx l
        unless (ty == t1) (Left ("expected " <> show t1 <> " but got " <> show ty))
        let t' = TSum ty t2 in pure (EIn (InL lE t') t')
      InL{} -> Left "left must be constructed with a sum type"
      InR r (TSum t1 t2) -> do
        rE@(EIn tm ty) <- check' ctx r
        unless (ty == t2) (Left ("expected " <> show t2 <> " but got " <> show ty))
        let t' = TSum t1 ty in pure (EIn (InR rE t') t')
      InR{} -> Left "right must be constructed with a sum type"
      Case e l r -> do
        leftE@(EIn _ fnTyL) <- check' ctx l
        rightE@(EIn _ fnTyR) <- check' ctx r
        caseE@(EIn tm scrutinee) <- check' ctx e
        case (fnTyL, fnTyR, scrutinee) of
          (TArr inTyL outTyL, TArr inTyR outTyR, TSum t1 t2)
            | outTyL == outTyR
            , inTyL == t1
            , inTyR == t2 -> Right (EIn (Case caseE leftE rightE) outTyL)
            | otherwise -> Left $ "nope " <> show scrutinee <> ". left is " <> show inTyL <> " -> " <> show outTyL
                                                            <> ". right is " <> show inTyR <> " -> " <> show outTyR <> "."
          (TArr inTyL outTyL, TArr inTyR outTyR, t) -> Left "don't know how to handle case with non-sum types"
          _ -> Left "both case patterns must be lambdas"
      App fn arg -> do
        fnE@(EIn fnTm fnTy) <- check' ctx fn
        argE@(EIn argTm argTy) <- check' ctx arg
        case fnTy of
          TArr inTy outTy | inTy == argTy -> Right (EIn (App fnE argE) outTy)
                          | otherwise -> Left $ "expected type " <> show inTy <> " but got " <> show argTy
          _ -> Left "expected a function type"
      Lam n ty body -> do
        bodyE@(EIn _ retTy) <- check' ((n, ty) : ctx) body
        pure (EIn (Lam n ty bodyE) (TArr ty retTy))



-- Implement in Syntax:
-- pairs
-- fst/snd
-- -> sums

-- Then, think about, study:
-- type inference
-- unification, hindley-milner (alcheck'rithm m, alcheck'rithm w)

-- Γ, _n_ : t |- Var n : t
-- Γ |- \ n : t -> e
-- -----------------
-- Γ, n : t |- e
