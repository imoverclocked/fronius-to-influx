{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module TestArchive (archiveDataTests) where

import Control.Monad (when)
import Data.List (isSuffixOf)
import F2I.Archive (processArchive)
import F2I.Common (ArchiveStatus (..), ProcessEntry)
import Streaming.Prelude qualified as SP
import Test.HUnit (Test (..), assertEqual)

archiveTarXZFixture :: String
archiveTarXZFixture = "tests/fixtures/20230302.tar.xz"

-- We do not care about actual decoding in the Archive code, only that it will
-- run a ProcessEntry over the contents and give us a stream of ArchiveStatus
processArchiveFilterExtPathNoop :: String -> ProcessEntry
processArchiveFilterExtPathNoop suffix path bs
    | suffix `isSuffixOf` path =
        ArchiveStatus
            { path = path,
              msg = "testPath NOOP",
              realFile = False,
              success = True,
              metrics = []
            }
    | otherwise =
        ArchiveStatus
            { path = path,
              msg = "ignore",
              realFile = False,
              success = False,
              metrics = []
            }

testTarXZArchive :: String -> Int -> Test
testTarXZArchive pathFilter numEntries =
    TestLabel ".tar.xz archive extraction" $
        TestLabel (pathFilter ++ " extraction") $
            TestCase
                ( do
                    let
                        archiveStream = processArchive archiveTarXZFixture (processArchiveFilterExtPathNoop pathFilter)
                        onlyFiles = SP.filter success archiveStream
                    archiveContainer : archiveDir : archiveList <- SP.toList_ onlyFiles
                    assertEqual "number of records" numEntries (length archiveList)
                    assertEqual "archive container path" archiveTarXZFixture (path archiveContainer)
                    assertEqual "archive container directory" "skipped non-file" (msg archiveDir)
                )

archiveDataTests :: Test
archiveDataTests =
    TestList
        [ testTarXZArchive ".powerflow" 759,
          testTarXZArchive ".inverter" 759,
          testTarXZArchive ".noSuchFile" 0
        ]
