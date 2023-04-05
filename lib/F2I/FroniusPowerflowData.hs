{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module F2I.FroniusPowerflowData (
    PowerflowBody (..),
    PowerflowEntry (..),
) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, withObject, (.:), (.=))
import Data.Aeson.Types (Parser, Value)
import Data.Kind (Type)
import Data.Map (Map)
import F2I.FroniusCommon (HeadData)
import GHC.Generics (Generic)
import Prelude (Int, Monad (return), Show, String, ($))

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
    { bodyPF :: PowerflowBody,
      headPF :: HeadData
    }
    deriving stock (Generic, Show)

instance FromJSON PowerflowEntry where
    parseJSON :: Value -> Parser PowerflowEntry
    parseJSON = withObject "PowerflowEntry" $ \v -> do
        bodyPF <- v .: "Body"
        headPF <- v .: "Head"
        return (PowerflowEntry {bodyPF = bodyPF, headPF = headPF})

instance ToJSON PowerflowEntry where
    toJSON :: PowerflowEntry -> Value
    toJSON (PowerflowEntry body headData) = object ["Body" .= body, "Head" .= headData]
