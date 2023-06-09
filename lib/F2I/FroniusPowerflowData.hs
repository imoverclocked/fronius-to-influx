{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module F2I.FroniusPowerflowData (
    PowerflowBody (..), -- export all so we can reuse for PowerflowAPIBodyData
    PowerflowEntry (PowerflowEntry),
    powerflowSiteMetrics,
    powerflowInverterMetrics,
) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, withObject, (.:), (.=))
import Data.Aeson.Types (Parser, Value)
import Data.Kind (Type)
import Data.Map (Map, toList)
import Data.Maybe (mapMaybe)
import F2I.Common (InfluxMetric (..), InfluxMetricGenerator (..), ProtoInfluxMetrics, ProtoMetricGenerator (protoMetrics))
import F2I.FroniusCommon (FroniusHeadData (baseTags, headData, headTimestamp, tagsFromHead), HeadData, defaultInfluxMetrics, maybeNumericValue, maybeStringValue)
import GHC.Generics (Generic)
import Prelude (Either (Left), Int, Monad (return), Show, String, ($), (++))

-- powerflow entry example
{-
   "Body" : {
      "Inverters" : {
         "1" : {
            "DT" : 168,
            "E_Day" : 1000,
            "E_Total" : 40603000,
            "E_Year" : 27000,
            "P" : 428
         }
      },
      "Site" : {
         "E_Day" : 1000,
         "E_Total" : 40603000,
         "E_Year" : 27000,
         "Meter_Location" : "unknown",
         "Mode" : "produce-only",
         "P_Akku" : null,
         "P_Grid" : null,
         "P_Load" : null,
         "P_PV" : 428,
         "rel_Autonomy" : null,
         "rel_SelfConsumption" : null
      },
      "Version" : "12"
   },
   "Head" : {
      "RequestArguments" : {},
      "Status" : {
         "Code" : 0,
         "Reason" : "",
         "UserMessage" : ""
      },
      "Timestamp" : "2023-03-11T11:59:45-08:00"
   }
-}

type PowerflowBody :: Type
data PowerflowBody = PowerflowBody
    { inverters :: Map String (Map String Int),
      site :: Map String Value,
      version :: String
    }
    deriving stock (Generic, Show)

instance ToJSON PowerflowBody where
    toJSON :: PowerflowBody -> Value
    toJSON (PowerflowBody inverters site version) =
        object
            [ "Inverters" .= inverters,
              "Site" .= site,
              "Version" .= version
            ]

instance FromJSON PowerflowBody where
    parseJSON :: Value -> Parser PowerflowBody
    parseJSON = withObject "PowerflowBody" $ \v -> do
        inverters <- v .: "Inverters"
        site <- v .: "Site"
        version <- v .: "Version"
        return (PowerflowBody {inverters = inverters, site = site, version = version})

type PowerflowEntry :: Type
data PowerflowEntry = PowerflowEntry
    { bodyF :: PowerflowBody,
      headF :: HeadData
    }
    deriving stock (Generic, Show)

instance FromJSON PowerflowEntry where
    parseJSON :: Value -> Parser PowerflowEntry
    parseJSON = withObject "PowerflowEntry" $ \v -> do
        bodyF <- v .: "Body"
        headF <- v .: "Head"
        return (PowerflowEntry {bodyF = bodyF, headF = headF})

instance ToJSON PowerflowEntry where
    toJSON :: PowerflowEntry -> Value
    toJSON (PowerflowEntry bodyF headF) = object ["Body" .= bodyF, "Head" .= headF]

instance FroniusHeadData PowerflowEntry where
    headData :: PowerflowEntry -> HeadData
    headData = headF

    baseTags :: PowerflowEntry -> [(String, String)]
    baseTags entry = ("version", version $ bodyF entry) : tagsFromHead entry

powerflowSiteMetrics :: Map String Value -> ProtoInfluxMetrics
powerflowSiteMetrics siteMap = do
    let
        siteTags =
            mapMaybe maybeStringValue $
                toList siteMap
                    ++ [("id", "site")]
        siteFields = mapMaybe maybeNumericValue $ toList siteMap

    [(siteTags, (k, Left v)) | (k, v) <- siteFields]

powerflowInverterMetrics :: Map String (Map String Int) -> ProtoInfluxMetrics
powerflowInverterMetrics inverters =
    [ ([("id", inverterId)], (k, Left v))
      | (inverterId, kv) <- toList inverters,
        (k, v) <- toList kv
    ]

instance ProtoMetricGenerator PowerflowEntry where
    protoMetrics :: PowerflowEntry -> ProtoInfluxMetrics
    protoMetrics entry = do
        let
            bodyData = bodyF entry
        powerflowSiteMetrics (site bodyData) ++ powerflowInverterMetrics (inverters bodyData)

instance InfluxMetricGenerator PowerflowEntry where
    measurementName :: PowerflowEntry -> String
    measurementName _ = "powerflow"

    influxMetrics :: PowerflowEntry -> [InfluxMetric]
    influxMetrics = defaultInfluxMetrics
