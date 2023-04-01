{-# OPTIONS_GHC -Wno-inferred-safe-imports #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Safe #-}

module Common(
    ArchiveStatus(..),
    ArchiveStatusStream,
    InfluxMetric(..),
    ProcessEntry,
) where

import Prelude (String, Either, Int, Bool, Maybe, Show, IO)
import Data.ByteString qualified as BS
import Data.Time ( UTCTime )
import Streaming.Prelude qualified as SP
import Data.Kind (Type)

type InfluxMetric :: Type
data InfluxMetric = InfluxMetric {
    measurement :: String,
    tags :: [(String, String)],
    field :: (String, Either Int Bool),
    timestamp :: Maybe UTCTime
} deriving stock (Show)

type ArchiveStatus :: Type
data ArchiveStatus = ArchiveStatus {
   path :: String,
   realFile :: Bool, -- does this path correspond to a file on the filesystem?
   success :: Bool,
   msg :: String,
   metrics :: [InfluxMetric]
} deriving stock (Show)

type ProcessEntry :: Type
type ProcessEntry = String -> BS.ByteString -> ArchiveStatus

type ArchiveStatusStream :: Type
type ArchiveStatusStream = SP.Stream (SP.Of ArchiveStatus) IO ()
