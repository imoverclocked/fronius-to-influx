{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
-- because aeson is unsafe
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module F2I.InfluxData (
    powerFlow,
    powerFlowFromBS,
    inverter,
    inverterFromBS,
) where

import Data.Aeson (eitherDecode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import F2I.Common (
    ArchiveStatus (ArchiveStatus, metrics, msg, path, realFile, success),
    ArchiveStatusStream,
    InfluxMetric (InfluxMetric, field, measurement, tags, timestamp),
    InfluxMetricGenerator (influxMetrics, measurementName),
 )
import F2I.FroniusInverterData (InverterEntry)
import F2I.FroniusPowerflowData (PowerflowEntry)
import Streaming qualified as S
import Streaming.Prelude qualified as SP
import Prelude (
    Applicative (pure),
    Bool (False, True),
    Either (Left, Right),
    Functor (fmap),
    IO,
    Int,
    String,
    ($),
    (++),
    (.),
    (=<<),
 )

-- Convert files/file contents to ArchiveStatus{...}

powerFlow :: String -> ArchiveStatusStream
powerFlow path = S.effect $ _powerFlow path

_powerFlow :: String -> IO ArchiveStatusStream
_powerFlow path = do
    bits <- BS.readFile path -- don't be lazy so we close handles sooner
    let
        archiveStatus = powerFlowFromBS path bits
    pure $ SP.yield archiveStatus {realFile = True}

powerFlowFromBS :: String -> BS.ByteString -> ArchiveStatus
powerFlowFromBS path content =
    _archiveStatusFromMetricGenerator
        path
        (eitherDecode $ BSL.fromStrict content :: Either String PowerflowEntry)

inverter :: String -> ArchiveStatusStream
inverter path = S.effect $ _inverter path

_inverter :: String -> IO ArchiveStatusStream
_inverter path = do
    bits <- BS.readFile path
    let
        archiveStatus = inverterFromBS path bits
    pure $ SP.yield archiveStatus {realFile = True}

inverterFromBS :: String -> BS.ByteString -> ArchiveStatus
inverterFromBS path content =
    _archiveStatusFromMetricGenerator
        path
        (eitherDecode $ BSL.fromStrict content :: Either String InverterEntry)

-- Left is an error message, Right is an InfluxMetricGenerator
_archiveStatusFromMetricGenerator :: (InfluxMetricGenerator a) => String -> Either String a -> ArchiveStatus
_archiveStatusFromMetricGenerator path (Left msg) =
    ArchiveStatus {path = path, success = False, msg = msg, realFile = False, metrics = []}
_archiveStatusFromMetricGenerator path (Right gen) =
    ArchiveStatus {path = path, success = True, msg = "", realFile = False, metrics = influxMetrics gen}
