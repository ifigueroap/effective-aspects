{-# LANGUAGE MultiParamTypeClasses,
             FlexibleContexts             
 #-}

module AOP.Test.AOT (tests) where

import AOP.Default
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Debug.Trace

tests = testGroup "AOT" [
          test_client1,
          test_client3,
          test_client4,
          test_client5,
          test_client6,
          test_client7,
          test_typc1,
          test_typc2,
          test_typc3,
          test_typc4,
          test_poly1,
          test_poly2,
          test_poly2',
          test_poly3',    
          test_poly4,
          test_poly6
        ]
--------------------------------------------------------------------------------

cat_a :: (Typeable1Monad m) => String -> m String
cat_a s = return (s ++ "a")

adv :: (Typeable1Monad m) => Advice m Integer Integer
adv proceed n = proceed (n+2)

adv2 :: (Typeable1Monad m) => Advice m Integer Integer
adv2 proceed _ = proceed 32

-- Type of programs and runner function
type AOID = AOT Identity
runAOID :: AOID a -> a
runAOID c = runIdentity (runAOT c)

successor :: Monad m => Integer -> m Integer
successor n = return (n+1)

successor2 :: Typeable1Monad m => Integer -> AOT m Integer
successor2 n = successor # n

successor3 :: Monad m => Integer -> m Integer
successor3 n = return (n+1)

successor'' :: Integer -> AOID Integer
successor'' n = return (n+1)

successor2'' :: Integer -> AOID Integer
successor2'' n = successor'' # n

client1 n = runAOID (program n)
program :: Integer -> AOID Integer
program n = let s = successor
            in do 
               deploy (aspect (pcCall s) adv)
               s # n
-- Tests
prop_client1 s = client1 s == runAOID (successor (s+2))
test_client1 = testProperty "Client 1" prop_client1

-- Problem with stable names because there are nested functions. A workaround is to use let bindings.
client3 n = runAOID (program3 n)
program3 :: Integer -> AOID Integer
program3 n = let s  = successor''  -- :: Integer -> AOID Integer
                 s2 = successor2'' -- :: Integer -> AOID Integer
             in do 
              deploy (aspect (pcCall successor'') adv2)
              successor2'' # n
-- Tests
prop_client3 n = client3 n == 33
test_client3 = testProperty "Client 3" prop_client3


-- Same problem with nested functions
client4 n = runAOID (program4 n)
program4 :: Integer -> AOID Integer
program4 n = let s = successor''
                 s2 = successor2''
             in do 
                deploy (aspect (pcCall successor'') adv)
                successor2'' # n
-- Tests
prop_client4 n = client4 n == runAOID (successor (n+2))
test_client4 = testProperty "Client 4" prop_client4


-- Testing undeploy
client5 n = runAOID (program5 n)
program5 :: Integer -> AOID Integer
program5 n = let s = successor 
             in do
                x <- s # n -- x = 4
                let a = aspect (pcCall s) (adv :: Advice AOID Integer Integer)
                deploy a
                y <- s # n -- y = 6
                undeploy a
                z <- s # n -- z = 1
                return (x + y + z)
-- Tests
prop_client5 n = client5 n == 2 * runAOID (successor n) + runAOID (successor(n+2))
test_client5 = testProperty "Client 5" prop_client5


adv6 :: (Typeable1Monad m) => Advice m Integer a
adv6 proceed n = proceed (n+2)

-- Testing pcType pointcut
client6 n = runAOID (program6 n)
program6 :: Integer -> AOID Integer
program6 n = do -- we specify the type signature using undefined :: sig. We could also define a macro in Template Haskell.
                -- the aspect applies on all functions that are more specific that the given type
                deploy (aspect (pcType (undefined :: Integer -> AOID a)) adv6)
                x <- successor # n
                y <- successor2 # n
                return (x+y)
-- Tests
prop_client6 n = client6 n == runAOID (successor (n+2)) + runAOID (successor (n+4))
test_client6 = testProperty "Client 6" prop_client6


client7 n = runAOID (program7 n)
program7 :: Integer -> AOID Integer
program7 n = let s  = successor
                 s2 = successor2
             in do -- all jps with the type, but pcCalls to successor2
                   deploy (aspect (pcType (undefined :: Integer -> AOID Integer) `pcAnd` pcNot (pcCall s2)) adv)
                   x <- s # n
                   y <- s2 # n
                   return (x+y)
-- Tests
prop_client7 n = client7 n == runAOID (successor (n+2)) + runAOID (successor (n+2))
test_client7 = testProperty "Client 7" prop_client7


{------ Testing pcType ------}
genericAdvice :: (Typeable1Monad m) => Advice m a b
genericAdvice proceed = proceed

-- pcType: concrete / advice: concrete
typc1 n = runAOID (typprogram1 n)
typprogram1 :: Integer -> AOID Integer
typprogram1 n = do
                 deploy (aspect (pcType (undefined :: Integer -> AOID Integer)) adv)
                 x <- successor # n  -- succ(n+2)
                 y <- successor2 # n -- succ(n+4) advice triggers for successor2 and its woven nested app of successor
                 return (x+y)        -- succ(n+2) + succ(n+4)
-- Tests
prop_typc1 n = typc1 n == runAOID (successor (n+2)) + runAOID (successor (n+4))
test_typc1 = testProperty "Type PC 1" prop_typc1


-- pcType: concrete / advice: polymorphic (with no constraints)
typc2 n = runAOID (typprogram2 n)
typprogram2 :: Integer -> AOID Integer
typprogram2 n = do
                  -- the generic advice cannot do much. We make it print something.
                  deploy (aspect (pcType (undefined :: Integer -> AOID Integer)) genericAdvice)
                  x <- successor # n  -- succ(n) 
                  y <- successor2 # n -- succ(n) advice triggers for successor2 and its woven nested app of successor
                  return (x+y) -- succ(n) + succ(n)
-- Tests
prop_typc2 n = typc2 n == 2 * runAOID (successor n)
test_typc2 = testProperty "Type PC 2" prop_typc2

strAdvice :: (Typeable1Monad m) => Advice m String b
strAdvice proceed arg = proceed (arg ++ "_advice_")

-- pcType: polymorphic / advice: polymorphic
typc3 n = runAOID (typprogram3 n)
typprogram3 :: Integer -> AOID Integer
typprogram3 n = do
                 deploy (aspect (pcType (undefined :: String -> AOID b)) strAdvice)
                 z <- cat_a # "hello"   -- hello_advice_a
                 x <- (successor :: Integer -> AOID Integer) # n
                 if z == "hello_advice_a"
                    then (return x)       -- 1 implies advice didn't trigger on successor2, as expected
                    else (return (-999)) -- error "Shouldn't happen"
-- Tests
prop_typc3 n = typc3 n == runAOID (successor n)
test_typc3 = testProperty "Type PC 3" prop_typc3


typc4 n = runAOID (typprogram4 n)
typprogram4 :: Integer -> AOID Integer
typprogram4 n = do -- advice should trigger 3 times
                  deploy (aspect (pcType (undefined :: a -> AOID b)) genericAdvice)
                  z <- cat_a # "hello"
                  successor2 # n
-- Tests
prop_typc4 n = typc4 n == runAOID (successor n)
test_typc4 = testProperty "Type PC 4" prop_typc4


{-------- Testing Parametric Polymorphism --------}

{- Advising a pcCall to a polymorphic function with a concrete advice.
   To do so, you must instantiate the pcCall to the polymorphic type so it 
   matches the type of the advice you want to deploy. -}
polyHead :: [a] -> AOID a
polyHead xs = return (head xs)

concreteAdvice :: Advice AOID [Integer] Integer
concreteAdvice proceed arg = proceed ([42] ++ arg)

-- not passing tests, maybe problem with pcCall pc
poly1 xs = runAOID (polyProgram1 xs)
polyProgram1 :: [Integer] -> AOID Integer
polyProgram1 xs = do 
                     deploy (aspect (pcCall (polyHead :: [Integer] -> AOID Integer)) concreteAdvice)
                     x <- polyHead # ([1, 2, 3] :: [Float])
                     polyHead # xs
-- Tests
prop_poly1 xs = poly1 xs == (42 :: Integer)
test_poly1 = testProperty "Polymorphism Case 1" prop_poly1


{- Advising a concrete function with a polymorphic advice.
   Case 1: using pcCall pc
   Case 2: using type pc
 -}
concreteFunction :: Integer -> AOID Integer
concreteFunction = return

concreteFunction2 :: Float -> AOID Float
concreteFunction2 = return

polyAdvice ::Advice AOID Integer Integer
polyAdvice proceed arg = proceed (arg+100)

-- using pcCall
poly2 n = runAOID (polyProgram2 n)
polyProgram2 :: Integer -> AOID Integer
polyProgram2 n = do
                   deploy (aspect (pcCall concreteFunction)  polyAdvice)
                   concreteFunction # n
-- Tests
prop_poly2 n = poly2 n == (n+100)
test_poly2 = testProperty "Polymorphism Case 2" prop_poly2

-- using pcType
-- This more general signature works OK with pcType
polyAdvice' :: (Typeable1Monad m, Num a, Show a) => Advice (AOT m) a b
polyAdvice' proceed arg = proceed (arg+100)

poly2' n = runAOID (polyProgram2' n)
polyProgram2' :: Integer -> AOID Integer
polyProgram2' n = do
                   deploy (aspect (pcType (undefined :: Integer -> AOID Integer)) polyAdvice')
                   concreteFunction # n -- n+1
-- Tests
prop_poly2' n = poly2' n == (n+100)
test_poly2' = testProperty "Polymorphism Case 2'" prop_poly2'


-- using pcType
poly3' x = runAOID (polyProgram3' x)
polyProgram3' :: Float -> AOID Float
polyProgram3' x = do deploy (aspect (pcType (undefined :: Float -> AOID Float)) polyAdvice')
                     concreteFunction2 # x                    
-- Tests
-- Since we are using floating point we establish a tolerance
prop_poly3' x = abs (poly3' x - (x+100)) < 0.001 where types = x :: Float
test_poly3' = testProperty "Polymorphism Case 3'" prop_poly3'


polyAdvice2 :: (Num a, Show a, Show b) => Advice AOID a b
polyAdvice2 proceed arg = proceed (arg +100)

constraintPoly x = runAOID (constraintPolyProgram x)
constraintPolyProgram :: Float -> AOID Float
constraintPolyProgram x = do deploy (aspect (pcCall concreteFunction2) polyAdvice2)
                             concreteFunction2 # x

-- Tests
prop_constraintPoly x = abs(constraintPoly x - (100+x)) < 0.0001 where types = x :: Float
test_constraintPoly = testProperty "Constrained Polymorphic Advice" prop_constraintPoly

concreteFunction3 :: Integer -> AOID String
concreteFunction3 n = return (show n) 

poly4 n = runAOID (polyProgram4 n)
polyProgram4 :: Integer -> AOID Integer
polyProgram4 n = do
                   deploy (aspect (pcCall concreteFunction)  polyAdvice)                   
                   concreteFunction # n
-- Tests
prop_poly4 n = poly4 n == (n+100)
test_poly4 = testProperty "Polymorphism Case 4" prop_poly4


{- Advising a pcCall to a polymorphic function with an also polymorphic advice.
   As usual, the type of the advice cannot be more specific than the type of the pointcut.

   Case 1: Using pcCall pc
   Case 2: Using type pc

   If the polymorphic target function has no constraints. It works OK. Else, see TestMacros.
 -}

polyHead2 :: (Typeable1Monad m, Num a) => [a] -> m a
polyHead2 xs = return (head xs)

polyAdvice3 :: (Typeable1Monad m, Num a) => Advice m [a] a
polyAdvice3 proceed arg = proceed ([47] ++ arg)

polyAdvice3' :: (Typeable1Monad m) => Advice (AOT m) [a] a
polyAdvice3' proceed arg = proceed (reverse arg)

-- Using pcType
-- If we omit the annotations the advice is not triggered.
poly5' :: [Integer] -> Integer
poly5' xs = runAOID (polyProgram5' xs)
polyProgram5' :: [Integer] -> AOID Integer
polyProgram5' xs = do
                     deploy (aspect (pcType (undefined :: [a] -> AOID a)) polyAdvice3')
                     polyHead2 # xs
-- Tests
prop_poly5' xs = not (null xs) ==> poly5' xs == last xs where types = xs :: [Integer]
test_poly5' = testProperty "Polymorphism Case 5'" prop_poly5'


-- When using pcCall we need a let binding
-- One for each specific type to be used
poly6 xs = runAOID (polyProgram6 xs)
polyProgram6 xs = let f = polyHead2 :: [Integer] -> AOID Integer
                      g = polyHead2 :: [Float] -> AOID Float
                  in do             
                      deploy (aspect (pcCall f) polyAdvice3)
                      deploy (aspect (pcCall g) polyAdvice3)
                      x <- f # ([1, 2, 3] :: [Integer])                      
                      y <- g # ([1, 2, 3] :: [Float])                     
                      z <- f # xs
                      -- Can't do wrong applications
                      -- w <- polyHead2 # ['a', 'b', 'c']
                      return (x + round y + z) -- 47 * 3 = 141
-- Tests
prop_poly6 xs = (xs /= []) ==> poly6 xs == 141
test_poly6 = testProperty "Polymorphism Case 6" prop_poly6
