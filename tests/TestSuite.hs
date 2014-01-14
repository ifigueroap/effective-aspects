module Main where

import Test.Framework (defaultMain)
import AOP.Test.AOT as AOT
-- import AOP.Test.Cflow as Cflow

main :: IO ()
main = defaultMain testSuite

testSuite =  [ AOT.tests ]

