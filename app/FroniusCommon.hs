{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module FroniusCommon(
    HeadData(..),
) where

import GHC.Generics (Generic)
import Data.Map (Map)
import Data.Aeson ((.:), (.=), withObject, object, FromJSON(parseJSON), ToJSON(toJSON), Value)

data HeadData = HeadData {
    requestArguments :: Map String String,
    status           :: Map String Value,
    timestamp        :: String
} deriving (Generic, Show)

instance FromJSON HeadData where
    parseJSON = withObject "HeadData" $ \v -> do
        requestArguments <-  v .: "RequestArguments"
        status <- v .: "Status"
        timestamp <- v .: "Timestamp"
        return (HeadData {requestArguments = requestArguments, status = status, timestamp = timestamp})

instance ToJSON HeadData where
    toJSON (HeadData requestArguments status timestamp) = object [
        "RequestArguments" .= requestArguments,
        "Status" .= status,
        "Timestamp" .= timestamp
        ]
