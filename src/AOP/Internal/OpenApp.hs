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

class Typeable1Monad m => OpenApp m where
  (#) :: (PolyTypeable (a -> m b)) => (a -> m b) -> a -> m b

class Typeable1Monad m => TaggedApp m where
  taggedApp :: (PolyTypeable (a -> m b)) => FunctionTag -> (a -> m b) -> a -> m b
