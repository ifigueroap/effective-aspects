{-# LANGUAGE FlexibleInstances,
             UndecidableInstances,
             ScopedTypeVariables,
             MultiParamTypeClasses
 #-}

-- | Default aspect semantics, aspects are deployed and undeployed at the top level.
module AOP.Default (module X) where

import AOP.Internal.JoinpointModel as X
import AOP.Internal.AOPMonad as X
import AOP.Internal.OpenApp as X
import AOP.Internal.PointcutLanguage as X
import AOP.Internal.AOT as X

-- | Default aspect semantics, aspects are deployed and undeployed at the top level.
instance (Typeable1Monad m, Typeable1Monad (t m)) => MonadDeploy t m where
  deployInEnv   asp aenv                                     = return (EAspect asp:aenv)
  undeployInEnv asp@(Aspect (pc::PC (t m) a b) adv hnd) aenv = return (deleteAsp (EAspect asp) aenv)
