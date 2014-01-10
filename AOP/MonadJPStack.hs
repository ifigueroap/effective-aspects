{-# LANGUAGE ExistentialQuantification
  #-}

module AOP.MonadJPStack (
 EJP(..), 
 JPStack,
 MonadJPStack(..),
) where

import AOP.Internal.JoinpointModel

data EJP = forall a b m. Monad m => EJP (Jp m a b)
type JPStack = [EJP]

class Monad m => MonadJPStack m where
      getJPStack  :: m JPStack
      pushJPStack :: EJP -> m ()
      popJPStack  :: m ()
