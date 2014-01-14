{-# LANGUAGE FlexibleContexts,
             ConstraintKinds,
             TypeFamilies,
             MultiParamTypeClasses,
             FlexibleInstances
 #-}


module AOP.Internal.PointcutLanguage (
 pcCall,
 pcType,
 pcAnd,
 pcTag,
 pcOr,
 pcNot, 
) where

import GHC.Prim (Constraint)
import AOP.Internal.JoinpointModel

import Debug.Trace

{- |
Built-in pointcuts pcCall and pcType, and pointcut combinators pcAnd, pcOr and pcNot.
Using typeclasses, pointcuts are open for new definitions of functions, like the Function wrapper.
We also define pcSeq, that matches a sequence of two join points.
-}

pcCall :: (Typeable1Monad m, PolyTypeable (a -> b)) => (a -> b) -> PC m a b
pcCall f = let typRefF = polyTypeOf f in PC (pcCallPred f typRefF defaultFunctionTag)
 where pcCallPred fun t tag = return $ \ jp -> return (compareFun fun tag jp && compareType t jp)

pcTag :: (Typeable1Monad m, PolyTypeable (a -> b)) => FunctionTag -> PC m a' b'
pcTag t = PC (pcTagPred t)
 where pcTagPred t = return $ \ (Jp _ tag _ _) -> return (tag == t)

pcType :: (Typeable1Monad m, PolyTypeable (a -> b)) => (a -> b) -> PC m a b
pcType f = let typRefF = polyTypeOf f in PC (pcTypePred typRefF)
 where pcTypePred t = (return (\jp -> return (compareType t jp)))

-- | And pointcut combinator, overloaded to support PC and RequirePC
class Typeable1Monad m => PCAnd m a1 b1 a2 b2 pct where
  type PCAndCtx m a1 b1 a2 b2 pct :: Constraint
  pcAnd :: PCAndCtx m a1 b1 a2 b2 pct => PC m a1 b1 -> pct m a2 b2 -> PC m a1 b1

-- | When combining two PC pointcuts, the matched types t1 and t2 must be the same
-- | This expressed in the constraint t1 ~ t2
instance Typeable1Monad m => PCAnd m a1 b1 a2 b2 PC where
  type PCAndCtx m a1 b1 a2 b2 PC = ((a1 -> b1) ~ (a2 -> b2))
  pcAnd (PC mpc1) (PC mpc2) = PC (_pcAndImpl mpc1 mpc2)

-- | When combining a PC with a RequirePC we constraint t1 to be LessGen than t2
instance Typeable1Monad m => PCAnd m a1 b1 a2 b2 RequirePC where
  type PCAndCtx m a1 b1 a2 b2 RequirePC = (LessGen (a1 -> b1) (a2 -> b2))
  pcAnd (PC mpc1) (RequirePC mpc2) = PC (_pcAndImpl mpc1 mpc2)

-- | Logical Or pointcut combinator.
pcOr :: (Typeable1Monad m, LeastGen (a1 -> b1) (a2 -> b2) (agen -> bgen)) => PC m a1 b1 -> PC m a2 b2 -> PC m agen bgen
pcOr (PC mpc1) (PC mpc2) = PC (_pcOrImpl mpc1 mpc2)

-- | Logical Not pointcut combinator.
pcNot :: Typeable1Monad m => PC m a1 b1 -> PC m a2 b2 
pcNot (PC mpc) = PC (_pcNotImpl mpc)


-- Implementations

_pcAndImpl mpc1 mpc2 = do
  pc1 <- mpc1
  pc2 <- mpc2
  return $ \ jp -> do
             res1 <- pc1 jp
             if res1
                then do res2 <- pc2 jp
                        return res2
                else return False

_pcOrImpl mpc1 mpc2 = do
  pc1 <- mpc1
  pc2 <- mpc2
  return $ \ jp -> do
             res1 <- pc1 jp
             if res1
                then return True
                else do res2 <- pc2 jp
                        return res2

_pcNotImpl mpc1 = do
  pc1 <- mpc1
  return $ \ jp -> do
    res1 <- pc1 jp
    return (not res1)

