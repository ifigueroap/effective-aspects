{-# LANGUAGE ExistentialQuantification,
             FlexibleInstances,
             MultiParamTypeClasses,
             ScopedTypeVariables,
             UndecidableInstances,
             FlexibleContexts
  #-}

module AOP.Cflow (
 pcCflow,
 pcCflowbelow,
 jpStackAspect,
 module AOP.MonadJPStack,
) where

import AOP.Internal.JoinpointModel
import AOP.Internal.StableNamesEq
import AOP.Internal.PolyTypeableUtils
import AOP.MonadJPStack

class PcCflow f where 
      pcCflow :: (Typeable1Monad m, MonadJPStack m, PolyTypeable (f a (m b))) =>
              f a (m b) -> PC m c (m' d)

instance PcCflow ((->)) where 
         pcCflow f = PC (_pcCflow f defaultFunctionTag)

-- | PcCflow/PcCflowbelow Pointcut Definition
_pcCflow f fTag = return (\_ -> do
         jpStack <- getJPStack
         let fType = polyTypeOf f
         return (any (isCallEJP f fTag) jpStack))

class PcCflowbelow f where 
      pcCflowbelow :: (Typeable1Monad m, MonadJPStack m, PolyTypeable (f a (m b))) =>
                    f a (m b) -> PC m c (m' d)

instance PcCflowbelow ((->)) where 
         pcCflowbelow f = PC (_pcCflowbelow f defaultFunctionTag)

_pcCflowbelow f fTag = return (\_ -> do
         jpStack <- getJPStack
         if null jpStack
            then return False
            else return (any (isCallEJP f fTag) (tail jpStack)))

-- | Uses the same criteria as pcCall to determine if a join point is in a given join point stack
isCallEJP f fTag ejp = let fType = polyTypeOf f in
         compareFunEJP f fTag ejp && compareTypeEJP fType ejp

-- | Comparing identity of functions:
-- | When given a join point with a regular function (signaled by the default tag)
-- | then we use StableNames for comparison. If the functions are wrapped,
-- | we compare the tags
compareFunEJP :: t -> FunctionTag -> EJP -> Bool
compareFunEJP f ft (EJP (Jp g t _ _)) = if t == defaultFunctionTag
                                then stableNamesEq f g
                                else ft == t

-- | Compare types to see if type representation t is less general 
-- | than the type of the function associated to the join point
compareTypeEJP :: TypeRep -> EJP -> Bool
compareTypeEJP t (EJP (Jp f _ _ _)) = isLessGeneral t (polyTypeOf f)


-- | Aspect that matches every join point and collects them in a join point stack.
jpStackAspect :: (Typeable1Monad m, MonadJPStack m) => Aspect m a (m b) a b
jpStackAspect = undefined -- aspect pcAny collectAdv

-- | Pointcut that matches every join point, and push them to the stack.
pcAny :: (Typeable1Monad m, MonadJPStack m) => PC m a b
pcAny = PC $ return $ \jp -> do
        pushJPStack (EJP jp)
        return True

-- | Advice that performs proceed, and then pops the join point stack.
collectAdv proceed arg = do
        result <- proceed arg
        popJPStack
        return result
