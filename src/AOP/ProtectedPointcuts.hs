{-# LANGUAGE ExistentialQuantification,
             FlexibleContexts
 #-}

module AOP.ProtectedPointcuts (
  ProtectedPC,
  protectPC,
  pAspect,
  ppcAnd,
  ppcOr
) where

import AOP.Internal.JoinpointModel
import AOP.Internal.PointcutLanguage (pcAnd, pcOr)

type Combinator t m a b = t -> Advice m a b

data ProtectedPC m a b t c d = (Typeable1Monad m, LessGen (a -> b) (c -> m d)) => PPC (PC m a b) (Combinator t m c d)

protectPC :: (Typeable1Monad m, LessGen (a -> b) (c -> m d)) => PC m a b -> Combinator t m c d -> ProtectedPC m a b t c d
protectPC = PPC

-- | Creates a protected aspect based on a given protected pc
pAspect :: (Monad m, LessGen (a -> b) (c -> m d)) => ProtectedPC m a b t c d -> t -> Aspect m a b c d
pAspect (PPC pc comb) as = aspect pc (comb as)

-- | Logical and combinator for protected pointcuts
ppcAnd :: (Typeable1Monad m) => ProtectedPC m a b t c d -> ProtectedPC m a b t c d -> ProtectedPC m a b t c d
ppcAnd (PPC pc1 comb) (PPC pc2 comd) = PPC (pcAnd pc1 pc2) comb

-- | Logical or combinator for protected pointcuts
ppcOr :: (Typeable1Monad m, LeastGen (a -> b) (c -> d) (agen -> bgen), LessGen (agen -> bgen) (a' -> m b')) 
      => ProtectedPC m a b t a' b' -> ProtectedPC m c d t a' b' -> ProtectedPC m agen bgen t a' b'
ppcOr (PPC pc1 comb) (PPC pc2 comd) = PPC (pcOr pc1 pc2) comb
