module AOP.Internal.StableNamesEq where

import System.Mem.StableName
import System.IO.Unsafe

-- | Equality of functions based on the StableName API
stableNamesEq :: a -> b -> Bool
stableNamesEq a b = unsafePerformIO $ do
                        pa <- makeStableName a
                        pb <- makeStableName b
                        return (hashStableName pa == hashStableName pb)
        
