{-# LANGUAGE ExistentialQuantification,
             FlexibleInstances,
             MultiParamTypeClasses,
             ScopedTypeVariables,
             UndecidableInstances,
             FlexibleContexts,
             GeneralizedNewtypeDeriving
  #-}

module AOP.JPStackT (
 JPStackT,
 evalJPStackT,
 module AOP.MonadJPStack,
) where


import AOP.Internal.JoinpointModel
import AOP.Internal.AOT
-- import AOP.Internal.StableNamesEq
-- import AOP.Internal.PolyTypeableUtils
import AOP.MonadJPStack

newtype JPStackT m a = JPStackT {run :: StateT JPStack m a}
        deriving (Functor, Monad, MonadPlus, MonadCont, MonadIO)

evalJPStackT :: Monad m => JPStackT m a -> JPStack -> m a
evalJPStackT c l = evalStateT (run c) l

instance Monad m => MonadJPStack (JPStackT m) where
      getJPStack     = JPStackT $ StateT $ (\s -> return (s, s))
      pushJPStack jp = JPStackT $ StateT $ (\s -> return ((), jp:s))
      popJPStack     = JPStackT $ StateT $ (\(jps) -> return ((), tail jps))

{- Interaction with AOT -}
instance (MonadJPStack m, Typeable1Monad (AOT m)) => MonadJPStack (AOT m) where
         getJPStack     = lift getJPStack
         pushJPStack jp = mkAOT $ \aenv -> do pushJPStack jp; return ((), aenv)
         popJPStack     = lift popJPStack

{- Instance for PolyTypeable compliance -}
instance Typeable1Monad m => Typeable1 (JPStackT m) where
         typeOf1 _ = mkTyConApp (mkTyCon3 "JPStackT" "JPStackT" "JPStackT")
                     [typeOf1 (undefined :: m ())]

instance MonadTrans JPStackT where
         lift ma = JPStackT $ StateT $ \ ctx -> do
                   a <- ma
                   return (a, ctx)

instance Monad m => MonadState s (JPStackT (StateT s m)) where
         get = lift get
         put s = lift (put s)

instance MonadWriter w m => MonadWriter w (JPStackT m) where
    tell     = lift . tell
    listen m = JPStackT $ StateT $ \s -> do
                 ((a, s'), w) <- listen (runStateT (run m) s)
                 return ((a, w), s')
    pass m   = JPStackT $ StateT $ \s -> pass $ do
                 ((a, f), s') <- runStateT (run m) s
                 return ((a, s'), f)

instance MonadReader r m => MonadReader r (JPStackT m) where
    ask       = lift ask
    local f m = JPStackT $ StateT $ \s -> local f (runStateT (run m) s)

