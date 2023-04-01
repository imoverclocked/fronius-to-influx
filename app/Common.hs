{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-inferred-safe-imports #-}

module Common (
    ArchiveStatus (..),
    ArchiveStatusStream,
    InfluxMetric (..),
    ProcessEntry,
) where

import Data.ByteString qualified as BS
import Data.Kind (Type)
import Data.Time (UTCTime)
import Streaming.Prelude qualified as SP
import Prelude (Bool, Either, IO, Int, Maybe, Show, String)

type InfluxMetric :: Type
data InfluxMetric = InfluxMetric
    { measurement :: String,
      tags :: [(String, String)],
      field :: (String, Either Int Bool),
      timestamp :: Maybe UTCTime
    }
    deriving stock (Show)

type ArchiveStatus :: Type
data ArchiveStatus = ArchiveStatus
    { path :: String,
      realFile :: Bool, -- does this path correspond to a file on the filesystem?
      success :: Bool,
      msg :: String,
      metrics :: [InfluxMetric]
    }
    deriving stock (Show)

type ProcessEntry :: Type
type ProcessEntry = String -> BS.ByteString -> ArchiveStatus

type ArchiveStatusStream :: Type
type ArchiveStatusStream = SP.Stream (SP.Of ArchiveStatus) IO ()
