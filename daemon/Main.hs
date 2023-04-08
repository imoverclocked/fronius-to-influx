{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception (displayException))
import Control.Monad (forever)
import Data.Aeson (FromJSON)
import Data.Data (Data)
import Data.Kind (Type)
import F2I.Common (InfluxMetricGenerator)
import F2I.FroniusInverterAPIData qualified as InverterAPI
import F2I.FroniusPowerflowAPIData qualified as PowerflowAPI
import F2I.InfluxConnection (sendStats)
import GHC.IO.Handle (hPutStr)
import GHC.IO.StdHandles (stderr)
import Network.HTTP.Client (Response, parseRequest)
import Network.HTTP.Simple (JSONException, getResponseBody, httpJSONEither)
import System.Console.CmdArgs (cmdArgs, details, help, summary, (&=))
import Prelude (Either (Left, Right), IO, Int, Maybe (Just, Nothing), Monad (return), Num ((*), (-)), Show, String, ($))

type FroniusToInfluxD :: Type
data FroniusToInfluxD = FroniusToInfluxD
    { influx_protocol :: String,
      host :: String,
      port :: Int,
      poll_interval :: Int,
      datamangerURI :: String
    }
    deriving stock (Show, Data)

defaultArgs :: FroniusToInfluxD
defaultArgs =
    FroniusToInfluxD
        { influx_protocol = "udp" &= help "connect via http, https or udp (default)",
          host = "127.0.0.1" &= help "default: 127.0.0.1",
          port = 8086 &= help "default: 8086",
          poll_interval = 10 &= help "seconds between polls",
          datamangerURI = "http://127.0.0.1:8080" &= help "the base datamanager URI"
        }
        &= help "Poll data from a Fronius Data Logger and send it to influxdb"
        &= summary "fronius-to-influxd (C) Tim Spriggs"
        &= details
            [ --    --------------------------------------------------------------------------------
              "fronius-to-influxd takes data from a live datamanager card and converts it to ",
              "influxdb for easier digestion of the data (eg: grafana)"
            ]

doRequest :: (FromJSON a) => String -> IO (Either JSONException a)
doRequest apiPath = do
    request <- parseRequest apiPath
    response :: Response (Either JSONException a) <- httpJSONEither request
    return $ getResponseBody response

printError :: JSONException -> IO ()
printError e = hPutStr stderr $ displayException e

handleMetric :: (InfluxMetricGenerator a) => FroniusToInfluxD -> Either JSONException a -> IO (Maybe a)
handleMetric _ (Left exception) = do
    printError exception
    return Nothing
handleMetric realArgs (Right res) = do
    sendStats (influx_protocol realArgs) (host realArgs) (port realArgs) $ Just res
    return $ Just res

metricRequest :: (FromJSON a, InfluxMetricGenerator a) => FroniusToInfluxD -> String -> IO (Maybe a)
metricRequest realArgs path = do
    m :: Either JSONException a <- doRequest path
    handleMetric realArgs m

main :: IO ()
main = do
    realArgs <- cmdArgs defaultArgs
    let
        baseURI = datamangerURI realArgs
        inverterURIPath = InverterAPI.apiPath baseURI
        powerFlowURIPath = PowerflowAPI.apiPath baseURI

    forever $ do
        _ :: Maybe InverterAPI.InverterAPIEntry <- metricRequest realArgs inverterURIPath
        threadDelay 100000 -- be nice to the API
        _ :: Maybe PowerflowAPI.PowerflowAPIEntry <- metricRequest realArgs powerFlowURIPath
        threadDelay (poll_interval realArgs * 1000000 - 100000)
