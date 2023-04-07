{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module F2I.FroniusInverterData (
    InverterStat (..),
    InverterEntry (..),
) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, withObject, (.:), (.=))
import Data.Aeson.Types (Parser, Value)
import Data.Kind (Type)
import Data.Map (Map, toList)
import F2I.Common (InfluxMetric (..), InfluxMetricGenerator (..), ProtoInfluxMetrics, ProtoMetricGenerator (protoMetrics))
import F2I.FroniusCommon (FroniusHeadData (baseTags, headData, headTimestamp, tagsFromHead), HeadData, defaultInfluxMetrics)
import GHC.Generics (Generic)
import Prelude (Either (Left), Int, Monad (return), Show, String, ($), (++))

-- Inverter Entry example
{-
  "Body" : {
      "DAY_ENERGY" : {
         "Unit" : "Wh",
         "Values" : {
            "1" : 631
         }
      },
      "PAC" : {
         "Unit" : "W",
         "Values" : {
            "1" : 0
         }
      },
      "TOTAL_ENERGY" : {
         "Unit" : "Wh",
         "Values" : {
            "1" : 40602000
         }
      },
      "YEAR_ENERGY" : {
         "Unit" : "Wh",
         "Values" : {
            "1" : 26000
         }
      }
   },
   "Head" : {
      "RequestArguments" : {
         "Query" : "Inverter",
         "Scope" : "System"
      },
      "Status" : {
         "Code" : 0,
         "Reason" : "",
         "UserMessage" : ""
      },
      "Timestamp" : "2023-03-11T07:37:23-08:00"
   }
-}

type InverterStat :: Type
data InverterStat = InverterStat
    { unit :: String,
      values :: Map String Int
    }
    deriving stock (Generic, Show)

instance FromJSON InverterStat where
    parseJSON :: Value -> Parser InverterStat
    parseJSON = withObject "InverterStat" $ \v -> do
        unit <- v .: "Unit"
        values <- v .: "Values"
        return (InverterStat {unit = unit, values = values})

instance ToJSON InverterStat where
    toJSON :: InverterStat -> Value
    toJSON (InverterStat unit values) = object ["Unit" .= unit, "Values" .= values]

type InverterEntry :: Type
data InverterEntry = InverterEntry
    { bodyIE :: Map String InverterStat,
      headIE :: HeadData
    }
    deriving stock (Generic, Show)

instance FromJSON InverterEntry where
    parseJSON :: Value -> Parser InverterEntry
    parseJSON = withObject "InverterEntry" $ \v -> do
        bodyIE <- v .: "Body"
        headIE <- v .: "Head"
        return (InverterEntry {bodyIE = bodyIE, headIE = headIE})

instance ToJSON InverterEntry where
    toJSON :: InverterEntry -> Value
    toJSON (InverterEntry bodyIE headIE) = object ["Body" .= bodyIE, "Head" .= headIE]

instance FroniusHeadData InverterEntry where
    headData :: InverterEntry -> HeadData
    headData = headIE

instance ProtoMetricGenerator InverterEntry where
    protoMetrics :: InverterEntry -> ProtoInfluxMetrics
    protoMetrics entry = do
        let
            bodyData = bodyIE entry
        [ ( [("id", i), ("unit", unit inverterStat)],
            (key, Left v1)
          )
          | (key, inverterStat) <- toList bodyData,
            (i, v1) <- toList $ values inverterStat
            ]

instance InfluxMetricGenerator InverterEntry where
    measurementName :: InverterEntry -> String
    measurementName _ = "inverter"

    influxMetrics :: InverterEntry -> [InfluxMetric]
    influxMetrics = defaultInfluxMetrics
