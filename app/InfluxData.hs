module InfluxData(
    powerFlow,
    powerFlowFromBS,
    inverter,
    inverterFromBS
) where

import Common
    ( ArchiveStatus(..),
      InfluxMetrics(InfluxMetrics, timestamp, measurement, tags,
                    fields) )
import Data.Aeson (decode, Value (Number))
import Data.Map (Map, toList)
import qualified Data.ByteString.Lazy as BSL
import FroniusCommon (HeadData(..), timestamp)
import FroniusPowerflowData ( PowerflowEntry(..), PowerflowBody(..) )
import Data.Time (UTCTime, zonedTimeToUTC)
import FroniusInverterData (InverterEntry(..), InverterStat(..))
import qualified FroniusPowerflowData as PowerflowEntry
import Data.Time.RFC3339 ( parseTimeRFC3339 )
import Data.Either (isLeft)
import Data.Scientific (toBoundedInteger)

 -- most head data is not useful in stats collection, populate this later if we find a need
tagsFromHead :: HeadData -> [(String, String)]
tagsFromHead _ = []

-- TODO: populate this
timestampFromHead :: HeadData -> Maybe UTCTime
timestampFromHead hD = fmap zonedTimeToUTC (parseTimeRFC3339 $ FroniusCommon.timestamp hD)

-- TODO: populate this
fieldsFromPFBody :: PowerflowBody -> [(String, Either Int Bool)]
fieldsFromPFBody pFBody =
    fieldsFromPFBodyInverters (inverters pFBody) ++
        fieldsFromPFBodySite (site pFBody)

fieldsFromPFBodyInverters :: Map String (Map String Int) -> [(String, Either Int Bool)]
fieldsFromPFBodyInverters invs = [
        (
            "Inverters." ++ inverterId ++ "." ++ statName,
            Left statValue
        ) | (inverterId, inverterStats) <- toList invs,
            (statName, statValue) <- toList inverterStats
    ]

-- hack! We don't care about non-numeric values in fields from Site
mapNumericValue :: (a, Value) -> (a, Either Int Bool)
mapNumericValue (k, Number n) = case toBoundedInteger n of
    (Just nInt) -> (k, Left nInt)
    _           -> (k, Right False)
mapNumericValue (k, _) = (k, Right False)

fieldsFromPFBodySite :: Map String Value -> [(String, Either Int Bool)]
fieldsFromPFBodySite sites =
    filter (\(_, b) -> isLeft b) -- Only keep numeric values (see hack note above)
        [ ("Site." ++ s, val) | (s, val) <- map mapNumericValue $ toList sites ]

fieldsFromInverterBody :: Map String InverterStat -> [(String, Either Int Bool)]
fieldsFromInverterBody invBody =
    concat [ fieldsFromValues k1 inverterStat | (k1, inverterStat) <- toList invBody ]

fieldsFromValues :: String -> InverterStat -> [(String, Either Int Bool)]
fieldsFromValues k1 inverterStat = do
    -- eg: DAY_ENERGY.Wh.
    let base = k1 ++ "." ++ unit inverterStat ++ "."
    -- append the inverter number to create a key: DAY_ENERGY.Wh.1
    [(base ++ i, Left v1) | (i, v1) <- toList $ values inverterStat]

powerFlow :: String -> IO ArchiveStatus
powerFlow path = do
    bits <- BSL.readFile path
    powerFlowFromBS path bits

powerFlowFromBS :: String -> BSL.ByteString -> IO ArchiveStatus
powerFlowFromBS path content = do
    let entry = decode content :: Maybe PowerflowEntry
    let headData = fmap PowerflowEntry.headPF entry
    let bodyData = fmap PowerflowEntry.bodyPF entry

    return ArchiveStatus{
        path = path,
        success = True,
        msg = "TODO: Process powerflow data",
        metrics = InfluxMetrics {
            measurement = "powerflow",
            tags = maybe [] tagsFromHead headData,
            fields = maybe [] fieldsFromPFBody bodyData,
            Common.timestamp = timestampFromHead =<< headData
            }
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

    return ArchiveStatus{
        path = path,
        success = True,
        msg = "TODO: Process inverter data",
        metrics = InfluxMetrics {
            measurement = "inverter",
            tags = maybe [] tagsFromHead headData,
            fields = maybe [] fieldsFromInverterBody bodyData,
            Common.timestamp = timestampFromHead =<< headData
            }
        }
