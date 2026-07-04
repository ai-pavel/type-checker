module Infer.Unify
  ( unify
  , unifyMany
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Infer.Types

-- | Unify two types, producing a substitution
unify :: Type -> Type -> Either TypeError Subst
unify (TFun l1 r1) (TFun l2 r2) = unifyMany [l1, r1] [l2, r2]
unify (TVar v) t = bind v t
unify t (TVar v) = bind v t
unify (TCon a) (TCon b)
  | a == b    = Right emptySubst
  | otherwise = Left (UnificationFail (TCon a) (TCon b))
unify (TTuple ts1) (TTuple ts2)
  | length ts1 == length ts2 = unifyMany ts1 ts2
  | otherwise = Left (UnificationFail (TTuple ts1) (TTuple ts2))
unify t1 t2 = Left (UnificationFail t1 t2)

-- | Unify corresponding pairs of types
unifyMany :: [Type] -> [Type] -> Either TypeError Subst
unifyMany [] [] = Right emptySubst
unifyMany (t1:ts1) (t2:ts2) = do
  s1 <- unify t1 t2
  s2 <- unifyMany (apply s1 ts1) (apply s1 ts2)
  return (composeSubst s2 s1)
unifyMany ts1 ts2 = Left (UnificationMismatch ts1 ts2)

-- | Bind a type variable to a type (with occurs check)
bind :: TVar -> Type -> Either TypeError Subst
bind v t
  | t == TVar v          = Right emptySubst
  | v `Set.member` ftv t = Left (InfiniteType v t)
  | otherwise            = Right (Map.singleton v t)
