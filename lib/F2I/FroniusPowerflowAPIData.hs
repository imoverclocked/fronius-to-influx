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
Module      : F2I.FroniusPowerflowAPIData
Description : Data structures that describe powerflow data from the Datamanager Web API
Stability   : experimental

The datamanager web API exposes a json data schema that is different from the data it
will publish via ftp.
-}
module F2I.FroniusPowerflowAPIData (
    apiPath,
    PowerflowAPIBodyData (..),
    PowerflowAPIEntry (..),
) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, withObject, (.:), (.=))
import Data.Aeson.Types (Parser, Value)
import Data.Kind (Type)
import Data.Map (Map)
import F2I.FroniusCommon (HeadData)
import F2I.FroniusPowerflowData (PowerflowBody)
import GHC.Generics (Generic)
import Prelude (Int, Monad (return), Show, String, ($))

apiPath :: String
apiPath = "/solar_api/v1/GetPowerFlowRealtimeData.fcgi"

-- Inverter Entry example
{-
   "Body" : {
      "Data" : {
         "Inverters" : {
            "1" : {
               "DT" : 168,
               "E_Day" : 7000,
               "E_Total" : 40709000,
               "E_Year" : 133000,
               "P" : 0
            }
         },
         "Site" : {
            "E_Day" : 7000,
            "E_Total" : 40709000,
            "E_Year" : 133000,
            "Meter_Location" : "unknown",
            "Mode" : "produce-only",
            "P_Akku" : null,
            "P_Grid" : null,
            "P_Load" : null,
            "P_PV" : null,
            "rel_Autonomy" : null,
            "rel_SelfConsumption" : null
         },
         "Version" : "12"
      }
   },
   "Head" : {
      "RequestArguments" : {},
      "Status" : {
         "Code" : 0,
         "Reason" : "",
         "UserMessage" : ""
      },
      "Timestamp" : "2023-04-06T20:57:05-07:00"
   }
-}

{- |
  Container for the "Inverters", "Site" and "Version" content
-}
type PowerflowAPIBodyData = PowerflowBody

{- |
  Container for the "Data" content
-}
type PowerflowAPIEntryBody :: Type
data PowerflowAPIEntryBody = PowerflowAPIEntryBody
    {dayta :: PowerflowAPIBodyData}
    deriving stock (Generic, Show)

instance FromJSON PowerflowAPIEntryBody where
    parseJSON :: Value -> Parser PowerflowAPIEntryBody
    parseJSON = withObject "PowerflowAPIEntryBody" $ \v -> do
        dayta <- v .: "Data"
        return (PowerflowAPIEntryBody {dayta = dayta})

instance ToJSON PowerflowAPIEntryBody where
    toJSON :: PowerflowAPIEntryBody -> Value
    toJSON (PowerflowAPIEntryBody dayta) = object ["Data" .= dayta]

{- |
  Top-level container for the "Body" and "Head" content
-}
type PowerflowAPIEntry :: Type
data PowerflowAPIEntry = PowerflowAPIEntry
    { bodyPFA :: PowerflowAPIEntryBody,
      headPFA :: HeadData
    }
    deriving stock (Generic, Show)

instance FromJSON PowerflowAPIEntry where
    parseJSON :: Value -> Parser PowerflowAPIEntry
    parseJSON = withObject "PowerflowAPIEntry" $ \v -> do
        bodyPFA <- v .: "Body"
        headPFA <- v .: "Head"
        return (PowerflowAPIEntry {bodyPFA = bodyPFA, headPFA = headPFA})

instance ToJSON PowerflowAPIEntry where
    toJSON :: PowerflowAPIEntry -> Value
    toJSON (PowerflowAPIEntry bodyPFA headPFA) = object ["Body" .= bodyPFA, "Head" .= headPFA]
