{-# LANGUAGE FlexibleInstances,
             UndecidableInstances,
             ScopedTypeVariables
  #-}

module AOP.Internal.Typeable1Monad (
 Typeable1Monad (..),
 module X,
) where

import Data.Typeable as X
import AOP.Internal.PolyTypeable as X
import Control.Monad as X
import Control.Monad.Identity as X
import Control.Monad.Trans as X
import Control.Monad.State as X
import Control.Monad.Writer as X
import Control.Monad.Reader as X
import Control.Monad.Cont as X
import Control.Monad.Error as X

{- | Support for PolyTypeable when using monads and monad transformers -}

class (Typeable1 m, Monad m) => Typeable1Monad m

instance (Typeable1 m, Monad m) => Typeable1Monad m

instance Typeable1 Identity where
         typeOf1 _ = mkTyConApp (mkTyCon3 "Control" "Monad" "Identity") []

instance (Typeable1Monad m, Typeable s) => Typeable1 (StateT s m) where
         typeOf1 _ = mkTyConApp (mkTyCon3 "Control" "Monad" "StateT") [typeOf (undefined :: s), typeOf1 (undefined :: m ())]

instance (Typeable1Monad m, Typeable s) => Typeable1 (WriterT s m) where
         typeOf1 _ = mkTyConApp (mkTyCon3 "Control" "Monad" "WriterT") [typeOf (undefined :: s), typeOf1 (undefined :: m ())]

instance (Typeable1Monad m, Typeable s) => Typeable1 (ErrorT s m) where
        typeOf1 _ = mkTyConApp (mkTyCon3 "Control" "Monad" "ErrorT")
                               [typeOf (undefined :: s), typeOf1 (undefined :: m ())]
