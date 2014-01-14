{-# LANGUAGE FlexibleContexts,
             ExistentialQuantification,
             ScopedTypeVariables,
             RankNTypes,
             TypeSynonymInstances,
             DeriveDataTypeable
 #-}

module AOP.Internal.JoinpointModel (
  -- | Join points
  FunctionTag,
  defaultFunctionTag,
  Jp (..),
  newjp,
  compareFun,
  compareType,
  getJpArg,

  -- Pointcuts
  PC (..),
  runPC,
  RequirePC (..),

  -- Advice
  Advice,
  applyAdv,

  -- Aspects
  Aspect (..),
  aspect,
  deleteAsp,
  EAspect (..),
  AspectEnv,  

  -- Other modules
  module X,
) where

import Unsafe.Coerce
import Control.Monad
import Data.Unique
import System.IO.Unsafe

import AOP.Internal.LessGen as X
import AOP.Internal.Typeable1Monad as X
import AOP.Internal.StableNamesEq
import AOP.Internal.PolyTypeableUtils

-- JOIN POINTS

type FunctionTag = Integer

defaultFunctionTag = 343123

-- | Join points are function applications. We store the function and the argument, and the function type representation.
-- | We add a FunctionTag value to use for quantification.
data Jp m a b = (Typeable1Monad m, PolyTypeable (a -> m b)) => Jp (a -> m b) FunctionTag a TypeRep

-- | Creates a join point with given function, tag, and argument
newjp :: (Typeable1Monad m, PolyTypeable (a -> m b)) => (a -> m b) -> FunctionTag -> a -> Jp m a b
newjp f t a = Jp f t a (polyTypeOf f)

-- | Comparing identity of functions:
compareFun :: (Typeable1Monad m, PolyTypeable (a -> m b)) => t -> FunctionTag -> Jp m a b -> Bool
compareFun f ft (Jp g t _ _) = if t == defaultFunctionTag then stableNamesEq f g else ft == t

-- | Compare types to see if type representation t is less general 
-- | than the type of the function associated to the join point
compareType :: (Typeable1Monad m, PolyTypeable (a -> m b)) => TypeRep -> Jp m a b -> Bool
compareType  t (Jp _ _ _ ft) = isLessGeneral ft t

-- | Gets the argument bound to the join point
getJpArg :: Monad m => Jp m a b -> a
getJpArg (Jp _ _ x _) = x

-- POINTCUTS

-- | A pointcut is a predicate on the current join point. It is used to identify join points of interest.
data PC m a b = PC {mpcond :: forall a' b'. m (Jp m a' b' -> m Bool)}

-- | Extracts the computation resulting of applying a join point to the pointcut
runPC (PC mpcond) jp = do { pccond <- mpcond; pccond jp}

-- | A RequirePC is not a valid standalone pointcut, it reflects a type requirement and must be combined with a standard PC.
data RequirePC m a b = Typeable1Monad m => RequirePC {mpcond' ::  forall a' b'. m (Jp m a' b' -> m Bool)}

-- | Support for PolyTypeable

instance (Typeable1 m) => Typeable2 (Jp m) where
  typeOf2 _ = mkTyConApp (mkTyCon3 "EffectiveAspects" "AOP.Internal.JoinpointModel" "Jp")
              [typeOf1 (undefined :: m ())]

instance (Typeable1 m) => Typeable2 (PC m) where
  typeOf2 _ = mkTyConApp (mkTyCon3 "PC" "PC" "PC") 
              [typeOf1 (undefined :: m ())]

instance (Typeable1 m) => Typeable2 (RequirePC m) where
  typeOf2 _ = mkTyConApp (mkTyCon3 "RequirePC" "RequirePC" "RequirePC") 
              [typeOf1 (undefined :: m ())]

-- ADVICE

type Advice m a b = (a -> m b) -> a -> m b

-- | Coerces t2 to be compatible with the advice. It passes t1 as a the proceed argument of the advice.
-- This coercion is safe, as described in the paper.
applyAdv :: Advice m a b -> t2 -> t2
applyAdv = unsafeCoerce


-- ASPECTS

type AspectHandle = Unique

-- | Typed first-class aspect. An aspect is tagged with a Unique value, used for identity
data Aspect m a b c d = LessGen (a -> b) (c -> m d) => Aspect (PC m a b) (Advice m c d) AspectHandle

newAspectHandle :: AspectHandle
newAspectHandle = unsafePerformIO newUnique

-- | Constructs a well-typed aspect
aspect :: (Typeable1Monad m, LessGen (a1 -> b1) (a2 -> m b2)) => PC m a1 b1 -> Advice m a2 b2 -> Aspect m a1 b1 a2 b2
aspect pc adv = Aspect pc adv newAspectHandle

-- | Aspect with hidden types, to be used in the aspect environment
data EAspect m = forall a b c d. LessGen (a -> b) (c -> m d) => EAspect (Aspect m a b c d)

-- | Aspect environment
type AspectEnv m = [EAspect m]

instance Show AspectHandle where
         show handle = show $ hashUnique handle

instance Show (Aspect m a b c d) where
         show (Aspect pc adv handle) = show handle

instance Show (EAspect m) where
         show (EAspect (Aspect pc adv handle)) = show handle

-- | Deletes asp from the aspect environment, used in undeploy
deleteAsp :: Typeable1Monad m => EAspect m -> AspectEnv m -> AspectEnv m
deleteAsp asp = filter (\asp' -> asp /= asp')

-- | Support for PolyTypeable
instance PolyTypeable Unique where
  polyTypeOf _ = mkTyConApp (mkTyCon3 "GHC" "Unique" "") []

-- | Notion of aspect equality to delete aspects from the aspect environment
instance Typeable1Monad m => Eq (EAspect m) where
  EAspect (Aspect _ _ u1) == EAspect (Aspect _ _ u2) = u1 == u2
