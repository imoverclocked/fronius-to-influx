{-# LANGUAGE DataKinds #-}
module Common(
    ArchiveStatus(..),
    ArchiveStatusStream,
    ProcessEntry,
    InfluxMetric(..)
) where

import qualified Data.ByteString as BS
import Data.Time ( UTCTime )
import qualified Streaming.Prelude as SP

data InfluxMetric = InfluxMetric {
    measurement :: String,
    tags :: [(String, String)],
    field :: (String, Either Int Bool),
    timestamp :: Maybe UTCTime
} deriving (Show)

data ArchiveStatus = ArchiveStatus {
   path :: String,
   realFile :: Bool, -- does this path correspond to a file on the filesystem?
   success :: Bool,
   msg :: String,
   metrics :: [InfluxMetric]
} deriving (Show)

type ProcessEntry = String -> BS.ByteString -> ArchiveStatus

type ArchiveStatusStream = SP.Stream (SP.Of ArchiveStatus) IO ()
