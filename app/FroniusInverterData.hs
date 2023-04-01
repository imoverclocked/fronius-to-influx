{-# OPTIONS_GHC -Wno-unsafe #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe #-}

module FroniusInverterData(
    InverterStat(..),
    InverterEntry(..)
) where

import Prelude (String, Int, Show, Monad (return), ($))
import GHC.Generics (Generic)
import Data.Map (Map)
import Data.Aeson ((.:), (.=), withObject, object, FromJSON(parseJSON), ToJSON(toJSON), Value)

import FroniusCommon ( HeadData )
import Data.Kind (Type)

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
data InverterStat = InverterStat {
    unit   :: String,
    values :: Map String Int
} deriving stock (Generic, Show)

instance FromJSON InverterStat where
    parseJSON = withObject "InverterStat" $ \v -> do
        unit <-  v .: "Unit"
        values <- v .: "Values"
        return (InverterStat {unit = unit, values = values})

instance ToJSON InverterStat where
    toJSON (InverterStat unit values) = object ["Unit" .= unit, "Values" .= values]


type InverterEntry :: Type
data InverterEntry = InverterEntry
    { bodyIE   :: Map String InverterStat,
      headIE   :: HeadData
    } deriving stock (Generic, Show)

instance FromJSON InverterEntry where
    parseJSON = withObject "InverterEntry" $ \v -> do
        bodyIE <-  v .: "Body"
        headIE <- v .: "Head"
        return (InverterEntry {bodyIE = bodyIE, headIE = headIE})

instance ToJSON InverterEntry where
    toJSON :: InverterEntry -> Value
    toJSON (InverterEntry bodyIE headIE) = object ["Body" .= bodyIE, "Head" .= headIE]
