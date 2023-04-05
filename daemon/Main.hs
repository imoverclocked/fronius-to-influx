{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Main (main) where

import Data.Data (Data)
import Data.Kind (Type)
import System.Console.CmdArgs (cmdArgs, details, help, summary, (&=))
import Prelude (IO, Int, Show, String, print)

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

main :: IO ()
main = do
    -- arg processing
    realArgs <- cmdArgs defaultArgs

    print realArgs
    print "TODO: write a daemon to poll a datamanager 2.0 and send metrics directly to influxdb"
