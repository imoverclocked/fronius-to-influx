{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
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
import F2I.FroniusInverterAPIData (InverterAPIEntry)
import F2I.FroniusInverterAPIData qualified as InverterAPI
import F2I.FroniusPowerflowAPIData (PowerflowAPIEntry)
import F2I.FroniusPowerflowAPIData qualified as PowerflowAPI
import F2I.InfluxConnection (sendStats)
import GHC.IO.Handle (hPutStr)
import GHC.IO.StdHandles (stderr)
import Network.HTTP.Client (Response, parseRequest)
import Network.HTTP.Simple (JSONException, getResponseBody, httpJSONEither)
import System.Console.CmdArgs (cmdArgs, details, help, summary, (&=))
import Prelude (Either (Left, Right), IO, Int, Maybe (Just), Monad (return), Num ((*), (-)), Show, String, ($), (++))

powerFlowURIPath :: String -> String
powerFlowURIPath base = base ++ PowerflowAPI.apiPath

inverterURIPath :: String -> String
inverterURIPath base = base ++ InverterAPI.apiPath

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

handleMetric :: (InfluxMetricGenerator a) => FroniusToInfluxD -> Either JSONException a -> IO ()
handleMetric _ (Left exception) = printError exception
handleMetric realArgs (Right res) = sendStats (influx_protocol realArgs) (host realArgs) (port realArgs) $ Just res

main :: IO ()
main = do
    realArgs <- cmdArgs defaultArgs
    forever $ do
        m1 :: Either JSONException PowerflowAPIEntry <- doRequest $ powerFlowURIPath $ datamangerURI realArgs
        _ <- handleMetric realArgs m1

        threadDelay 100000 -- be nice to the API
        m2 :: Either JSONException InverterAPIEntry <- doRequest $ inverterURIPath $ datamangerURI realArgs
        _ <- handleMetric realArgs m2

        threadDelay (poll_interval realArgs * 1000000 - 100000)
