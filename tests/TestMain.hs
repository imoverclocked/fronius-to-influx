module Main where

import System.Exit     ( exitFailure, exitSuccess )
import Test.HUnit      ( runTestTT, Counts(failures, errors), Test(TestList) )

import TestFroniusData ( froniusDataTests )

tests :: Test
tests = TestList [
    froniusDataTests
    ]

main :: IO Counts
main = do
    counts <- runTestTT tests
    if errors counts + failures counts == 0
        then exitSuccess
        else exitFailure
