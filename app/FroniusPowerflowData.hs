{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module FroniusPowerflowData(
    PowerflowBody(..),
    PowerflowEntry(..)
) where

import GHC.Generics (Generic)
import Data.Map (Map)
import Data.Aeson ((.:), (.=), withObject, object, FromJSON(parseJSON), ToJSON(toJSON), Value)

import FroniusCommon (HeadData(..))

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

data PowerflowBody = PowerflowBody
    { inverters :: Map String (Map String Int),
      site      :: Map String Value,
      version   :: String
    } deriving (Generic, Show)

instance ToJSON PowerflowBody where
    toJSON (PowerflowBody inverters site version) = object
      [ "Inverters" .= inverters,
        "Site" .= site,
        "Version" .= version ]

instance FromJSON PowerflowBody where
    parseJSON = withObject "PowerflowBody" $ \v -> do
        inverters <-  v .: "Inverters"
        site <- v .: "Site"
        version <- v .: "Version"
        return (PowerflowBody {inverters = inverters, site = site, version = version})


data PowerflowEntry = PowerflowEntry
    { bodyPF :: PowerflowBody,
      headPF :: HeadData
    } deriving (Generic, Show)

instance FromJSON PowerflowEntry where
    parseJSON = withObject "PowerflowEntry" $ \v -> do
        bodyPF <-  v .: "Body"
        headPF <- v .: "Head"
        return (PowerflowEntry {bodyPF = bodyPF, headPF = headPF})

instance ToJSON PowerflowEntry where
    toJSON (PowerflowEntry body headData) = object ["Body" .= body, "Head" .= headData]

