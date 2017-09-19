module Infer where

import Syntax
import Pretty
import Text.PrettyPrint (render)
import Control.Monad.Except
import Control.Monad.State
import Data.Monoid hiding (Sum)
import qualified Data.Map as Map
import Data.List (nub)
import Data.Set ((\\))
import qualified Data.Set as Set
import qualified Data.List as List

-- Typing context/environment implemented as a map of variable name to type scheme.
newtype TypeEnv = TypeEnv (Map.Map Name Scheme)

type Subst = Map.Map Name Type

-- Monad transformer stack of ExceptT and State. ExceptT allows easy error
-- reporting and State keeps track of a unique, incremening Int which we use to
-- keep track of fresh type variables as needed.
type Infer a = ExceptT String (State Int) a

infer :: Expr -> Result Scheme
infer = runInfer . infer' (TypeEnv Map.empty)
  where
    infer' :: TypeEnv -> Expr -> Infer (Subst, Type)
    infer' env expr = case out expr of
      Var x -> lookupEnv env x
      Lit (LInt _) -> pure (Map.empty, TInt)
      Lit (LBool _) -> pure (Map.empty, TBool)
      Lam x _ body -> do
        tv <- fresh
        let env' = extend env (x, Forall [] tv)
        (s1, t1) <- infer' env' body
        pure (s1, apply s1 (TArr tv t1))
      App e1 e2 -> do
        tv <- fresh
        (s1, t1) <- infer' env e1
        (s2, t2) <- infer' (apply s1 env) e2
        s3 <- unify (apply s2 t1) (TArr t2 tv)
        pure (s3 `compose` s2 `compose` s1, apply s3 tv)


-- Extend the environment with a new variable name an associated type scheme.
extend :: TypeEnv -> (Name, Scheme) -> TypeEnv
extend (TypeEnv env) (x, s) = TypeEnv $ Map.insert x s env

runInfer :: Infer (Subst, Type) -> Result Scheme
runInfer m = fmap closeOver (evalState (runExceptT m) 0)
  where
    -- specializing
    closeOver :: (Map.Map TName Type, Type) -> Scheme
    closeOver (sub, ty) = normalize (generalize (apply sub ty))

    -- Normalizes the free type variables back to the beginning of the set of fresh letters.
    normalize :: Scheme -> Scheme
    normalize (Forall as t) = Forall (fmap snd ord) (normType t)
      where
        ord = zip (nub (fv t)) letters

        fv (TVar a) = [a]
        fv (TArr a b) = fv a <> fv b
        fv TInt = []
        fv TBool = []

        normType (TVar a) = maybe (error ("type variable " <> a <> " not in signature")) TVar (List.lookup a ord)
        normType (TArr a b) = TArr (normType a) (normType b)
        normType TInt = TInt
        normType TBool = TBool

    generalize :: Type -> Scheme
    generalize t = Forall (Set.toList (ftv t)) t


-- Compose substitutions.
compose :: Subst -> Subst -> Subst
compose s1 s2 = Map.union (Map.map (apply s1) s2) s1

-- Unify two types to produce a substitution.
unify :: Type -> Type -> Infer Subst
unify (TArr l r) (TArr l' r') = do
  s1 <- unify l l'
  s2 <- unify (apply s1 r) (apply s1 r')
  pure (s2 `compose` s1)
unify (TVar a) t = bind a t
unify t (TVar a) = bind a t
unify TInt TInt = pure Map.empty
unify TBool TBool = pure Map.empty
unify t1 t2 = throwError ("failed to unify " <> render (ppr 0 t1) <> " with " <> render (ppr 0 t2))

bind :: TName -> Type -> Infer Subst
bind a t | t == TVar a = pure Map.empty
         | Set.member a (ftv t) = throwError ("cannot construct the infinite type " <> a <> " = " <> render (ppr 0 t))
         | otherwise = pure (Map.singleton a t)

lookupEnv :: TypeEnv -> Name -> Infer (Subst, Type)
lookupEnv (TypeEnv env) x = case Map.lookup x env of
  Nothing -> throwError ("free variable " <> x)
  Just s -> instantiate s >>= pure . (,) Map.empty
  where
    instantiate :: Scheme -> Infer Type
    instantiate (Forall as t) = do
      as' <- traverse (const fresh) as
      let s = Map.fromList (zip as as')
      pure (apply s t)

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

-- lam :: (Term -> Term) -> Term
-- lam f = Lam n body
--   where n = succ (maxBoundVariable body)
--         body = f (var n)
--
-- identity = lam id

fresh :: Infer Type
fresh = do
  i <- get
  put (succ i)
  pure $ TVar (letters !! i)

-- Model substituion of type variables -> types.
class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set.Set TName

instance Substitutable Type where
  apply s t@(TVar a) = Map.findWithDefault t a s
  apply s (TArr t1 t2) = TArr (apply s t1) (apply s t2)
  apply _ TInt = TInt
  apply _ TBool = TBool

  ftv (TVar a) = Set.singleton a
  ftv (TArr t1 t2) = Set.union (ftv t1) (ftv t2)
  ftv TInt = Set.empty
  ftv TBool = Set.empty

instance Substitutable Scheme where
  apply s (Forall as t) = Forall as (apply (foldr Map.delete s as) t)
  ftv (Forall as t) = ftv t \\ Set.fromList as

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  ftv = foldr (Set.union . ftv) Set.empty

instance Substitutable TypeEnv where
  apply s (TypeEnv env) = TypeEnv (Map.map (apply s) env)
  ftv (TypeEnv env) = ftv (Map.elems env)
