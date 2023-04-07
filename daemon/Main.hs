{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Aeson (FromJSON)
import Data.Data (Data)
import Data.Kind (Type)
import F2I.FroniusInverterAPIData (InverterAPIEntry)
import F2I.FroniusInverterAPIData qualified as InverterAPI
import F2I.FroniusPowerflowAPIData (PowerflowAPIEntry)
import F2I.FroniusPowerflowAPIData qualified as PowerflowAPI
import Network.HTTP.Client (Response, parseRequest)
import Network.HTTP.Simple (JSONException, getResponseBody, httpJSONEither)
import System.Console.CmdArgs (cmdArgs, details, help, summary, (&=))
import Prelude (Either, IO, Int, Monad (return), Num ((*)), Show, String, print, ($), (++))

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

doRequest :: forall a. (FromJSON a) => String -> IO (Either JSONException a)
doRequest apiPath = do
    request <- parseRequest apiPath
    response <- httpJSONEither request :: IO (Response (Either JSONException a))
    return $ getResponseBody response

main :: IO ()
main = do
    realArgs <- cmdArgs defaultArgs
    print "TODO: write a daemon to poll a datamanager 2.0 and send metrics directly to influxdb"

    forever $ do
        inverterEither :: Either JSONException InverterAPIEntry <- doRequest $ inverterURIPath $ datamangerURI realArgs
        print inverterEither

        threadDelay (5 * 1000000)

        powerflowEither :: Either JSONException PowerflowAPIEntry <- doRequest $ powerFlowURIPath $ datamangerURI realArgs
        print powerflowEither

        threadDelay (5 * 1000000)
