{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}
-- because aeson
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module F2I.FroniusCommon (
    HeadData (..),
) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, withObject, (.:), (.=))
import Data.Aeson.Types (Parser, Value)
import Data.Kind (Type)
import Data.Map (Map)
import GHC.Generics (Generic)
import Prelude (Monad (return), Show, String, ($))

type HeadData :: Type
data HeadData = HeadData
    { requestArguments :: Map String String,
      status :: Map String Value,
      timestamp :: String
    }
    deriving stock (Generic, Show)

instance FromJSON HeadData where
    parseJSON :: Value -> Parser HeadData
    parseJSON = withObject "HeadData" $ \v -> do
        requestArguments <- v .: "RequestArguments"
        status <- v .: "Status"
        timestamp <- v .: "Timestamp"
        return (HeadData {requestArguments = requestArguments, status = status, timestamp = timestamp})

instance ToJSON HeadData where
    toJSON :: HeadData -> Value
    toJSON (HeadData requestArguments status timestamp) =
        object
            [ "RequestArguments" .= requestArguments,
              "Status" .= status,
              "Timestamp" .= timestamp
            ]
