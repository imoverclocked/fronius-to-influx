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
    InverterAPIEntryBody (..),
    InverterAPIEntry (..),
) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, withObject, (.:), (.=))
import Data.Aeson.Types (Parser, Value)
import Data.Kind (Type)
import Data.Map (Map)
import F2I.FroniusCommon (HeadData)
import GHC.Generics (Generic)
import Prelude (Int, Monad (return), Show, String, ($))

apiPath :: String
apiPath = "/solar_api/v1/GetInverterInfo.cgi"

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
      daytah :: Map String (Map String Value)
    }
    deriving stock (Generic, Show)

instance FromJSON InverterAPIEntryBody where
    parseJSON :: Value -> Parser InverterAPIEntryBody
    parseJSON = withObject "InverterAPIEntryBody" $ \v -> do
        daytah <- v .: "Data"
        return (InverterAPIEntryBody {daytah = daytah})

instance ToJSON InverterAPIEntryBody where
    toJSON :: InverterAPIEntryBody -> Value
    toJSON (InverterAPIEntryBody dayta) = object ["Data" .= dayta]

{- |
  Top-level container
-}
type InverterAPIEntry :: Type
data InverterAPIEntry = InverterAPIEntry
    { bodyIAE :: InverterAPIEntryBody,
      headIAE :: HeadData
    }
    deriving stock (Generic, Show)

instance FromJSON InverterAPIEntry where
    parseJSON :: Value -> Parser InverterAPIEntry
    parseJSON = withObject "InverterAPIEntry" $ \v -> do
        bodyIAE <- v .: "Body"
        headIAE <- v .: "Head"
        return (InverterAPIEntry {bodyIAE = bodyIAE, headIAE = headIAE})

instance ToJSON InverterAPIEntry where
    toJSON :: InverterAPIEntry -> Value
    toJSON (InverterAPIEntry bodyIAE headIAE) = object ["Body" .= bodyIAE, "Head" .= headIAE]
