{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

{-# HLINT ignore "Use newtype instead of data" #-}

{- |
Module      : F2I.FroniusInverterAPIData
Description : Data structures that describe inverter data from the Datamanager Web API
Stability   : experimental

The datamanager web API exposes a json data schema that is different from the data it
will publish via ftp.
-}
module F2I.FroniusInverterAPIData (
    apiPath,
    InverterAPIEntryBody (InverterAPIEntryBody),
    InverterAPIEntry (InverterAPIEntry),
) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, withObject, (.:), (.=))
import Data.Aeson.Types (Parser, Value)
import Data.Kind (Type)
import Data.Map (Map, toList)
import Data.Maybe (mapMaybe)
import F2I.Common (InfluxMetric, InfluxMetricGenerator (..), ProtoInfluxMetrics, ProtoMetricGenerator (protoMetrics))
import F2I.FroniusCommon (FroniusHeadData (..), HeadData, defaultInfluxMetrics, maybeNumericValue)
import GHC.Generics (Generic)
import Prelude (Either (Left), Int, Monad (return), Show, String, ($), (++), (.))

apiPath :: String -> String
apiPath base = base ++ "/solar_api/v1/GetInverterInfo.cgi"

-- Inverter Entry example
{-
   "Body" : {
      "Data" : {
         "1" : {
            "CustomName" : "",
            "DT" : 168,
            "ErrorCode" : 0,
            "PVPower" : 3800,
            "Show" : 1,
            "StatusCode" : -1,
            "UniqueID" : "195714"
         }
      }
   },
   "Head" : {
      "RequestArguments" : {},
      "Status" : {
         "Code" : 0,
         "Reason" : "",
         "UserMessage" : ""
      },
      "Timestamp" : "2023-04-06T21:43:53-07:00"
   }
-}

{- |
  Container for the "Body" content
-}
type InverterAPIEntryBody :: Type
data InverterAPIEntryBody = InverterAPIEntryBody
    { -- | Inverter Name/Number -> Stat Name -> Stat Value
      dataF :: Map String (Map String Value)
    }
    deriving stock (Generic, Show)

instance FromJSON InverterAPIEntryBody where
    parseJSON :: Value -> Parser InverterAPIEntryBody
    parseJSON = withObject "InverterAPIEntryBody" $ \v -> do
        dataF <- v .: "Data"
        return (InverterAPIEntryBody {dataF = dataF})

instance ToJSON InverterAPIEntryBody where
    toJSON :: InverterAPIEntryBody -> Value
    toJSON (InverterAPIEntryBody dataF) = object ["Data" .= dataF]

{- |
  Top-level container
-}
type InverterAPIEntry :: Type
data InverterAPIEntry = InverterAPIEntry
    { bodyF :: InverterAPIEntryBody,
      headF :: HeadData
    }
    deriving stock (Generic, Show)

instance FromJSON InverterAPIEntry where
    parseJSON :: Value -> Parser InverterAPIEntry
    parseJSON = withObject "InverterAPIEntry" $ \v -> do
        bodyF <- v .: "Body"
        headF <- v .: "Head"
        return (InverterAPIEntry {bodyF = bodyF, headF = headF})

instance ToJSON InverterAPIEntry where
    toJSON :: InverterAPIEntry -> Value
    toJSON (InverterAPIEntry bodyF headF) = object ["Body" .= bodyF, "Head" .= headF]

instance FroniusHeadData InverterAPIEntry where
    headData :: InverterAPIEntry -> HeadData
    headData = headF

instance ProtoMetricGenerator InverterAPIEntry where
    protoMetrics :: InverterAPIEntry -> ProtoInfluxMetrics
    protoMetrics entry = do
        let
            bodyData = dataF . bodyF $ entry :: Map String (Map String Value)
        [ ( [("id", i)],
            (key, Left v1)
          )
          | (i, inverterStat) <- toList bodyData,
            (key, v1) <- mapMaybe maybeNumericValue $ toList inverterStat
            ]

instance InfluxMetricGenerator InverterAPIEntry where
    measurementName :: InverterAPIEntry -> String
    measurementName _ = "inverter"

    influxMetrics :: InverterAPIEntry -> [InfluxMetric]
    influxMetrics = defaultInfluxMetrics
