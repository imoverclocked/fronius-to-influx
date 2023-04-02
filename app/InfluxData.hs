{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
-- because aeson is unsafe
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module InfluxData (
    powerFlow,
    powerFlowFromBS,
    inverter,
    inverterFromBS,
) where

import Common (
    ArchiveStatus (ArchiveStatus, metrics, msg, path, realFile, success),
    ArchiveStatusStream,
    InfluxMetric (InfluxMetric, field, measurement, tags, timestamp),
 )
import Data.Aeson (Value (Number, String), decode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Map (Map, toList)
import Data.Maybe (Maybe (Just, Nothing), mapMaybe, maybe)
import Data.Scientific (toBoundedInteger)
import Data.Text (unpack)
import Data.Time (UTCTime, zonedTimeToUTC)
import Data.Time.RFC3339 (parseTimeRFC3339)
import FroniusCommon (HeadData, timestamp)
import FroniusInverterData (InverterEntry (bodyIE, headIE), InverterStat (unit, values))
import FroniusPowerflowData (PowerflowBody (inverters, site, version), PowerflowEntry)
import FroniusPowerflowData qualified as PowerflowEntry
import Streaming qualified as S
import Streaming.Prelude qualified as SP
import Prelude (
    Applicative (pure),
    Bool (False, True),
    Either (Left),
    Functor (fmap),
    IO,
    Int,
    String,
    ($),
    (++),
    (=<<),
 )

-- most head data is not useful in stats collection, populate this later if we find a need
tagsFromHead :: HeadData -> [(String, String)]
tagsFromHead _ = []

timestampFromHead :: HeadData -> Maybe UTCTime
timestampFromHead hD = fmap zonedTimeToUTC (parseTimeRFC3339 $ FroniusCommon.timestamp hD)

maybeNumericValue :: (a, Value) -> Maybe (a, Int)
maybeNumericValue (l, Number n) = case toBoundedInteger n of
    (Just nInt) -> Just (l, nInt)
    _ -> Nothing
maybeNumericValue _ = Nothing

maybeStringValue :: (a, Value) -> Maybe (a, String)
maybeStringValue (l, String str) = Just (l, unpack str)
maybeStringValue _ = Nothing

powerflowSiteMetrics :: Map String Value -> [([(String, String)], (String, Either Int Bool))]
powerflowSiteMetrics siteMap = do
    let
        siteTags =
            mapMaybe maybeStringValue $
                toList siteMap
                    ++ [("id", "site")]
        siteFields = mapMaybe maybeNumericValue $ toList siteMap

    [(siteTags, (k, Left v)) | (k, v) <- siteFields]

powerflowInverterMetrics :: Map String (Map String Int) -> [([(String, String)], (String, Either Int Bool))]
powerflowInverterMetrics inverters =
    [ ([("id", inverterId)], (k, Left v))
      | (inverterId, kv) <- toList inverters,
        (k, v) <- toList kv
    ]

generatePowerflowMetrics :: Maybe UTCTime -> [(String, String)] -> PowerflowBody -> [InfluxMetric]
generatePowerflowMetrics timestamp baseTags pfBody =
    [ InfluxMetric
        { measurement = "powerflow",
          tags = baseTags ++ [("version", version pfBody)] ++ pFTags,
          field = field,
          Common.timestamp = timestamp
        }
      | (pFTags, field) <-
            powerflowSiteMetrics (site pfBody) ++ powerflowInverterMetrics (inverters pfBody)
    ]

-- TODO: cleanup types: produces a list of ( list of tags, inverter stat )
inverterMetricsFromBody :: Map String InverterStat -> [([(String, String)], (String, Either Int Bool))]
inverterMetricsFromBody inverterStats =
    [ ( [("id", i), ("unit", unit inverterStat)],
        (key, Left v1)
      )
      | (key, inverterStat) <- toList inverterStats,
        (i, v1) <- toList $ values inverterStat
    ]

generateInverterMetrics :: Maybe UTCTime -> [(String, String)] -> Map String InverterStat -> [InfluxMetric]
generateInverterMetrics timestamp baseTags inverterBody =
    [ InfluxMetric
        { measurement = "inverter",
          tags = baseTags ++ iTags,
          field = field,
          Common.timestamp = timestamp
        }
      | (iTags, field) <- inverterMetricsFromBody inverterBody
    ]

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
powerFlowFromBS path content = do
    let
        entry = decode $ BSL.fromStrict content :: Maybe PowerflowEntry
        headData = fmap PowerflowEntry.headPF entry
        bodyData = fmap PowerflowEntry.bodyPF entry
        headTags = maybe [] tagsFromHead headData
        timestamp = timestampFromHead =<< headData
        metrics = maybe [] (generatePowerflowMetrics timestamp headTags) bodyData

    ArchiveStatus
        { path = path,
          realFile = False,
          success = True,
          msg = "",
          metrics = metrics
        }

inverter :: String -> ArchiveStatusStream
inverter path = S.effect $ _inverter path

_inverter :: String -> IO ArchiveStatusStream
_inverter path = do
    bits <- BS.readFile path
    let
        archiveStatus = inverterFromBS path bits
    pure $ SP.yield archiveStatus {realFile = True}

inverterFromBS :: String -> BS.ByteString -> ArchiveStatus
inverterFromBS path content = do
    let
        entry = decode $ BSL.fromStrict content :: Maybe InverterEntry
        headData = fmap FroniusInverterData.headIE entry
        bodyData = fmap FroniusInverterData.bodyIE entry
        headTags = maybe [] tagsFromHead headData
        timestamp = timestampFromHead =<< headData
        inverterMetrics = maybe [] (generateInverterMetrics timestamp headTags) bodyData

    ArchiveStatus
        { path = path,
          realFile = False,
          success = True,
          msg = "",
          metrics = inverterMetrics
        }
