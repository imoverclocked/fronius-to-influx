{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module InfluxConnection(
    sendStats
) where

import Common ( InfluxMetric(timestamp, measurement, tags, field) )
import Network.Socket ( SockAddr, Socket )
import Network.Run.UDP ( runUDPClient )

import Database.InfluxDB ( Line(Line), Field(FieldBool, FieldInt), HasServer (server) )
import Database.InfluxDB.Types ( Measurement, Key, LineField, Server (..) )
import qualified Database.InfluxDB.Write as HTTP
import qualified Database.InfluxDB.Write.UDP as UDP
import Data.String ( IsString(fromString) )
import Data.Map ( Map, fromList )
import System.Exit (die)
import Control.Lens ((.~), (&))
import Data.Time (UTCTime)

getUDPConnection :: String -> Int -> (Socket -> SockAddr -> IO m) -> IO m
getUDPConnection host port = runUDPClient host (show port)

writeUDPMetrics :: [(InfluxMetric, n)] -> UDP.WriteParams -> IO [n]
writeUDPMetrics metricPairs params = do
    -- write the metric (potentially into the UDP void)
    let filtered = [(lineFromMetricPair (metric, foo), foo) | (metric, foo) <- metricPairs, metricFilter metric]
    sequence_ [UDP.write params $ fst metric | metric <- filtered]
    -- Can't do this because https://github.com/maoe/influxdb-haskell/issues/92
    -- UDP.writeBatch params $ [fst pair | pair <- filtered]
    return [snd pair | pair <- filtered]

doUDPClient :: [(InfluxMetric, n)] -> Socket -> SockAddr -> IO [n]
doUDPClient metricResultPairs sock sockAddr = do
    let params = UDP.writeParams sock sockAddr
    writeUDPMetrics metricResultPairs params

lineFromMetricPair :: (InfluxMetric, b) -> Line UTCTime
lineFromMetricPair (metric, _) = do
    -- Type wrangling
    let m = fromString (measurement metric) :: Measurement
    let t = Data.Map.fromList [ (fromString k, fromString v) | (k, v) <- tags metric ]
    let f = Data.Map.fromList [ case field metric of
                                    (k, Left s) -> (fromString k, FieldInt $ fromIntegral s)
                                    (k, Right b) -> (fromString k, FieldBool b)
                                ] :: Map Key LineField
    let ts = timestamp metric

    Line m t f ts

metricFilter :: InfluxMetric -> Bool
metricFilter metric = not $ null $ measurement metric

writeHTTPMetrics :: (Show n) => [(InfluxMetric, n)] -> HTTP.WriteParams -> IO [n]
writeHTTPMetrics metricPairs params = do
    HTTP.writeBatch params $ [lineFromMetricPair pair | pair <- metricPairs, metricFilter $ fst pair ]
    return [snd pair | pair <- metricPairs]

doHTTPClient :: (Show n) => String -> Int -> Bool -> [(InfluxMetric, n)] -> IO [n]
doHTTPClient host port ssl metricResultPairs = do
    let params = HTTP.writeParams "fronius" & server .~ Server{
        _host = fromString host,
        _port = port,
        _ssl = ssl
    } 

    writeHTTPMetrics metricResultPairs params

{--
  Takes a list of (InfluxMetric, n) and sends them to a host:port
  returns a list of [n] that was sent (maybe unsuccessfully)
-}
sendStats :: (Show n) => String -> String -> Int -> [(InfluxMetric, n)] -> IO [n]
sendStats "udp" host port f = getUDPConnection host port (doUDPClient f)
sendStats "http" host port f = doHTTPClient host port False f
sendStats "https" host port f = doHTTPClient host port True f
sendStats unknown _ _ _ = die $ "Unknown protocol: " ++ unknown
