{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-inferred-safe-imports #-}

module F2I.Common (
    ArchiveStatus (..),
    ArchiveStatusStream,
    InfluxMetric (..),
    InfluxMetricGenerator (..),
    ProcessEntry,
    ProtoInfluxMetrics,
    ProtoMetricGenerator (..),
) where

import Data.ByteString qualified as BS
import Data.Kind (Type)
import Data.Time (UTCTime)
import Streaming.Prelude qualified as SP
import Prelude (Bool, Either, Eq, IO, Int, Maybe, Show, String, (++))

class InfluxMetricGenerator a where
    measurementName :: a -> String
    influxMetrics :: a -> [InfluxMetric]

{- |
[ [ (tag key, value) ], (measurement, value) ]
-}
type ProtoInfluxMetrics = [([(String, String)], (String, Either Int Bool))]

-- | A useful mid-step before actual metric generation
class ProtoMetricGenerator a where
    protoMetrics :: a -> ProtoInfluxMetrics

type InfluxMetric :: Type
data InfluxMetric = InfluxMetric
    { measurement :: String,
      tags :: [(String, String)],
      field :: (String, Either Int Bool),
      timestamp :: Maybe UTCTime
    }
    deriving stock (Eq, Show)

type ArchiveStatus :: Type
data ArchiveStatus = ArchiveStatus
    { path :: String,
      realFile :: Bool, -- does this path correspond to a file on the filesystem?
      success :: Bool,
      msg :: String,
      metrics :: [InfluxMetric]
    }
    deriving stock (Eq, Show)

type ProcessEntry :: Type
type ProcessEntry = String -> BS.ByteString -> ArchiveStatus

type ArchiveStatusStream :: Type
type ArchiveStatusStream = SP.Stream (SP.Of ArchiveStatus) IO ()
