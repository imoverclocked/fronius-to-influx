{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}
-- because aeson
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module F2I.FroniusCommon (
    FroniusHeadData (..),
    HeadData (..),
    defaultInfluxMetrics,
    maybeNumericValue,
    maybeStringValue,
) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (Number, String), object, withObject, (.:), (.=))
import Data.Aeson.Types (Parser, Value)
import Data.Kind (Type)
import Data.Map (Map)
import Data.Scientific (toBoundedInteger)
import Data.Text (unpack)
import Data.Time (UTCTime, zonedTimeToUTC)
import Data.Time.RFC3339 (parseTimeRFC3339)
import F2I.Common (InfluxMetric (InfluxMetric, field, measurement, tags), InfluxMetricGenerator (measurementName), ProtoMetricGenerator (protoMetrics))
import F2I.Common qualified as F2IC
import GHC.Generics (Generic)
import Prelude (Bool, Either, Functor (fmap), Int, Maybe (Just, Nothing), Monad (return), Show, String, ($), (++))

class FroniusHeadData a where
    headData :: a -> HeadData

    headTimestamp :: a -> Maybe UTCTime
    headTimestamp entry = fmap zonedTimeToUTC (parseTimeRFC3339 $ F2I.FroniusCommon.timestamp $ headData entry)

    -- most head data is not useful in stats collection, populate this later if we find a need
    tagsFromHead :: a -> [(String, String)]
    tagsFromHead _ = []

    -- useful to override while still keeping the promise of tagsFromHead
    baseTags :: a -> [(String, String)]
    baseTags = tagsFromHead

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

-- | Filter for Int valued tuples
maybeNumericValue :: (a, Value) -> Maybe (a, Int)
maybeNumericValue (l, Number n) = case toBoundedInteger n of
    (Just nInt) -> Just (l, nInt)
    _ -> Nothing
maybeNumericValue _ = Nothing

-- | Filter for String valued tuples
maybeStringValue :: (a, Value) -> Maybe (a, String)
maybeStringValue (l, String str) = Just (l, unpack str)
maybeStringValue _ = Nothing

-- There is probably a better way, I don't know it yet.
defaultInfluxMetrics :: (InfluxMetricGenerator a, ProtoMetricGenerator a, FroniusHeadData a) => a -> [InfluxMetric]
defaultInfluxMetrics entry =
    [ InfluxMetric
        { measurement = measurementName entry,
          tags = baseTags entry ++ pFTags,
          field = field,
          F2IC.timestamp = headTimestamp entry
        }
      | (pFTags, field) <- protoMetrics entry
    ]
