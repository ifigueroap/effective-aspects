{-# LANGUAGE TemplateHaskell,
             ScopedTypeVariables,
             FlexibleContexts
  #-}

module AOP.Test.Cflow (tests) where

import AOP.Default
import AOP.JPStackT
import AOP.Cflow

import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck


-- Test Suite
tests = testGroup "Control Flow" [test_client1, test_client2, test_client3]

-------------------------------------------------------------------------------- 

runProg c = runIdentity (evalJPStackT (runAOT_sp (SP pcFalse []) c) [])

-- Fresh tags for our notion of function identity
sucTag = $newTag
sucTag2 = $newTag

successor  = mkFunction (\n -> return (n+1)) sucTag

successor2 = mkFunction (\n -> successor # n) sucTag2


adv  proceed n = proceed (n+2)
adv2 proceed n = proceed 32

-- In this test the pcCflowbelow pointcut triggers adv2
client1 n = runProg (program1 n)
program1 n = do              
              deploy (aspect (pcCall successor `pcAnd` pcCflowbelow successor2) adv2)
              deploy jpStackAspect
              successor2 # n
--Tests
prop_client1 (n::Integer) = client1 n == 33
test_client1 = testProperty "Client 1" prop_client1

-- In this test the pcCflow pointcut triggers adv
client2 n = runProg (program2 n)
program2 n = do               
              deploy (aspect (pcCall successor `pcAnd` pcCflow successor) adv)
              deploy jpStackAspect
              successor2 # n
-- Tests
prop_client2 n = client2 n == runProg (successor2 # (n+2))
test_client2 = testProperty "Client 2" prop_client2


-- In this tests the advice is not triggered
client3 n = runProg (program3 n)
program3 n = do               
              deploy (aspect (pcCall successor `pcAnd` pcCflowbelow successor) adv)
              deploy jpStackAspect
              successor2 # n
-- Tests
prop_client3 n = client3 n == runProg (successor2 # n)
test_client3 = testProperty "Client 3" prop_client3

