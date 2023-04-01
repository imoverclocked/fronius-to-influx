{-# OPTIONS_GHC -Wno-unsafe #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe #-} -- because aeson

module FroniusCommon(
    HeadData(..),
) where

import Prelude (String, Show, Monad (return), ($))
import GHC.Generics (Generic)
import Data.Map (Map)
import Data.Aeson ((.:), (.=), withObject, object, FromJSON(parseJSON), ToJSON(toJSON))
import Data.Aeson.Types (Parser, Value)
import Data.Kind (Type)

type HeadData :: Type
data HeadData = HeadData {
    requestArguments :: Map String String,
    status           :: Map String Value,
    timestamp        :: String
} deriving stock (Generic, Show)

instance FromJSON HeadData where
    parseJSON :: Value -> Parser HeadData
    parseJSON = withObject "HeadData" $ \v -> do
        requestArguments <-  v .: "RequestArguments"
        status <- v .: "Status"
        timestamp <- v .: "Timestamp"
        return (HeadData {requestArguments = requestArguments, status = status, timestamp = timestamp})

instance ToJSON HeadData where
    toJSON :: HeadData -> Value
    toJSON (HeadData requestArguments status timestamp) = object [
        "RequestArguments" .= requestArguments,
        "Status" .= status,
        "Timestamp" .= timestamp
        ]
