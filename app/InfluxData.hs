{-# LANGUAGE OverloadedStrings #-}

module InfluxData(
    powerFlow,
    powerFlowFromBS,
    inverter,
    inverterFromBS
) where

import Common ( ArchiveStatus(..), InfluxMetric(..) )
import Data.Aeson (decode, Value (Number, String))
import Data.Map (Map, toList)
import qualified Data.ByteString.Lazy as BSL
import FroniusCommon (HeadData(..), timestamp)
import FroniusPowerflowData ( PowerflowEntry(..), PowerflowBody(..) )
import Data.Time (UTCTime, zonedTimeToUTC)
import FroniusInverterData (InverterEntry(..), InverterStat(..))
import qualified FroniusPowerflowData as PowerflowEntry
import Data.Time.RFC3339 ( parseTimeRFC3339 )
import Data.Scientific (toBoundedInteger)
import Data.Text (unpack)
import Data.Maybe (mapMaybe)

 -- most head data is not useful in stats collection, populate this later if we find a need
tagsFromHead :: HeadData -> [(String, String)]
tagsFromHead _ = []

timestampFromHead :: HeadData -> Maybe UTCTime
timestampFromHead hD = fmap zonedTimeToUTC (parseTimeRFC3339 $ FroniusCommon.timestamp hD)

maybeNumericValue :: (a, Value) -> Maybe (a, Int)
maybeNumericValue (l, Number n) = case toBoundedInteger n of
    (Just nInt) -> Just (l, nInt)
    _           -> Nothing
maybeNumericValue _ = Nothing

maybeStringValue :: (a, Value) -> Maybe (a, String)
maybeStringValue (l, String str) = Just (l, unpack str)
maybeStringValue _ = Nothing

powerflowSiteMetrics :: Map String Value -> [( [(String, String)], (String, Either Int Bool) )]
powerflowSiteMetrics siteMap = do
    let siteTags = mapMaybe maybeStringValue $ toList siteMap
            ++ [("id", "site")]
    let siteFields = mapMaybe maybeNumericValue $ toList siteMap

    [ (siteTags, (k, Left v)) | (k, v) <- siteFields ]

powerflowInverterMetrics :: Map String (Map String Int) -> [( [(String, String)], (String, Either Int Bool) )]
powerflowInverterMetrics inverters = [
    ([("id", inverterId)], (k, Left v))
    | (inverterId, kv) <- toList inverters,
    (k, v) <- toList kv
    ]

generatePowerflowMetrics :: Maybe UTCTime -> [(String, String)] -> PowerflowBody -> [InfluxMetric]
generatePowerflowMetrics timestamp baseTags pfBody = [
        InfluxMetric{
            measurement = "powerflow",
            tags = baseTags ++ [("version", version pfBody)] ++ pFTags,
            field = field,
            Common.timestamp = timestamp
        } | (pFTags, field) <-
            powerflowSiteMetrics (site pfBody) ++ powerflowInverterMetrics (inverters pfBody)
    ]

-- TODO: cleanup types: produces a list of ( list of tags, inverter stat )
inverterMetricsFromBody :: Map String InverterStat -> [ ([(String, String)], (String, Either Int Bool) )]
inverterMetricsFromBody inverterStats = [
        (
            [("id", i), ("unit", unit inverterStat)],
            (key, Left v1)
        ) |
        (key, inverterStat) <- toList inverterStats,
        (i, v1) <- toList $ values inverterStat
    ]

generateInverterMetrics :: Maybe UTCTime -> [(String, String)] -> Map String InverterStat -> [InfluxMetric]
generateInverterMetrics timestamp baseTags inverterBody = [
    InfluxMetric{
        measurement = "inverter",
        tags = baseTags ++ iTags,
        field = field,
        Common.timestamp = timestamp
    } | (iTags, field) <- inverterMetricsFromBody inverterBody
    ]

-- Convert files/file contents to ArchiveStatus{...}

powerFlow :: String -> IO ArchiveStatus
powerFlow path = do
    bits <- BSL.readFile path
    powerFlowFromBS path bits

powerFlowFromBS :: String -> BSL.ByteString -> IO ArchiveStatus
powerFlowFromBS path content = do
    let entry = decode content :: Maybe PowerflowEntry
    let headData = fmap PowerflowEntry.headPF entry
    let bodyData = fmap PowerflowEntry.bodyPF entry
    let headTags = maybe [] tagsFromHead headData
    let timestamp = timestampFromHead =<< headData

    let metrics = maybe [] (generatePowerflowMetrics timestamp headTags) bodyData

    return ArchiveStatus{
        path = path,
        success = True,
        msg = "",
        metrics = metrics
        }

inverter :: String -> IO ArchiveStatus
inverter path = do
    bits <- BSL.readFile path
    inverterFromBS path bits

inverterFromBS :: String -> BSL.ByteString -> IO ArchiveStatus
inverterFromBS path content = do
    let entry = decode content :: Maybe InverterEntry
    let headData = fmap FroniusInverterData.headIE entry
    let bodyData = fmap FroniusInverterData.bodyIE entry

    let headTags = maybe [] tagsFromHead headData
    let timestamp = timestampFromHead =<< headData
    let inverterMetrics = maybe [] (generateInverterMetrics timestamp headTags) bodyData

    return ArchiveStatus{
        path = path,
        success = True,
        msg = "",
        metrics = inverterMetrics
        }
