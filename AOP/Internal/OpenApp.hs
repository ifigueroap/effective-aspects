{-# LANGUAGE FlexibleContexts,
             KindSignatures,
             MultiParamTypeClasses,
             ImplicitParams  
 #-}

module AOP.Internal.OpenApp (
 OpenApp   (..),
 TaggedApp (..),
) where

import AOP.Internal.Typeable1Monad
import AOP.Internal.JoinpointModel (FunctionTag)

{- |
Open function application. # is defined as a typeclass to make it
extensible to different function representations.

f is a function-like type constructor that receives an argument 
and return types. For simplicity this does not appear in the paper.

The returned value is always a regular (woven) function.
-}

class Typeable1Monad m => OpenApp (f :: * -> * -> *) m where
  (#) :: (PolyTypeable (f a (m b)), PolyTypeable (a -> m b)) => f a (m b) -> a -> m b

class Typeable1Monad m => TaggedApp (f :: * -> * -> *) m where
  taggedApp :: (PolyTypeable (f a (m b)), PolyTypeable (a -> m b)) => FunctionTag -> f a (m b) -> a -> m b
