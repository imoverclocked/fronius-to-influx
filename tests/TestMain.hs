module Main where

import System.Exit (exitFailure, exitSuccess)
import Test.HUnit (Counts (errors, failures), Test (TestList), runTestTT)
import TestArchive (archiveDataTests)
import TestFroniusData (froniusDataTests)

tests :: Test
tests =
    TestList
        [ froniusDataTests,
          archiveDataTests
        ]

main :: IO Counts
main = do
    counts <- runTestTT tests
    if errors counts + failures counts == 0
        then exitSuccess
        else exitFailure
