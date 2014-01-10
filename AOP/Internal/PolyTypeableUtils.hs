{-# LANGUAGE FlexibleInstances #-}

module AOP.Internal.PolyTypeableUtils where

import AOP.Internal.PolyTypeable
import Data.Typeable.Internal
import System.IO.Unsafe
import Data.HashTable
import Data.Char

-- | Monad andmap
mmap :: Monad m => (a -> a' -> m Bool) -> [a] -> [a'] -> m Bool
mmap _ [] []         = return True
mmap _ [] _          = return False
mmap _ _ []          = return False
mmap f (h:t) (h':t') = do x <- f h h'
                          y <- mmap f t t'
                          return (x && y)

-- | Applies a function on the first component of a pair
first :: (a -> a') -> (a,b) -> (a',b)
first f (a,b) = (f a, b)

-- | Returns True iff t1 is less general than t2
-- | We use a hashmap to compute a one-way unification from t1 to t2
isLessGeneral :: TypeRep -> TypeRep -> Bool
isLessGeneral t1 t2 = unsafePerformIO $
                                do hash <- new (==) hashInt  
                                   oneWayUnification hash t1 t2
  where oneWayUnification hash t1 t2 = case first tyConName (splitTyConApp t1) of
                    (tc1,  []) -> if head tc1 == 'a' 
                                     then do 
                                             let int_tc1 = digitToInt (last tc1)
                                             is_registered <- Data.HashTable.lookup hash int_tc1 
                                             case is_registered of
                                                Nothing -> do 
                                                              insert hash int_tc1 t2 
                                                              return True       
                                                Just t2' -> return (t2 == t2')
                                     else  
                                       return (t1 == t2)
                    (tc1, l1) -> case first tyConName (splitTyConApp t2) of
                                    (tc2, l2) -> do b <- mmap (oneWayUnification hash) l1 l2
                                                    return (tc1 == tc2 && b)
