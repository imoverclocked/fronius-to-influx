{-# LANGUAGE DataKinds #-}
module InfluxConnection(
    sendStats
) where

import Common ( InfluxMetrics(timestamp, measurement, tags, fields) )
import Network.Socket ( SockAddr, Socket )
import Network.Run.UDP ( runUDPClient )

import Database.InfluxDB ( Line(Line), Field(FieldBool, FieldInt) )
import Database.InfluxDB.Types ( Measurement, Key, LineField )
import qualified Database.InfluxDB.Write.UDP as UDP
import Data.String ( IsString(fromString) )
import Data.Map ( Map, fromList )
import Control.Monad (unless)

-- There are no other immeditely supported connection types so we'll just
-- hard code udp everywhere for now.
getUDPConnection :: String -> Int -> (Socket -> SockAddr -> IO m) -> IO m
getUDPConnection host port = runUDPClient host (show port)

writeMetrics :: (InfluxMetrics, n) -> UDP.WriteParams -> IO n
writeMetrics (metric, foo) params = do
    -- Type wrangling
    let m = fromString (measurement metric) :: Measurement
    let t = Data.Map.fromList [ (fromString k, fromString v) | (k, v) <- tags metric ]
    let f = Data.Map.fromList [ case v of
                                    Left s -> (fromString k, FieldInt $ fromIntegral s)
                                    Right b -> (fromString k, FieldBool b)
                                | (k, v) <- fields metric
                                ] :: Map Key LineField
    let ts = timestamp metric

    -- write the metric (potentially into the UDP void)
    unless (null f) $ do
        UDP.write params (Line m t f ts)

    return foo

doClient ::  [(InfluxMetrics, n)] -> Socket -> SockAddr -> IO [n]
doClient metricResultPairs sock sockAddr = do
    let params = UDP.writeParams sock sockAddr

    -- [IO n]
    let iOresults = [ writeMetrics (metric, result) params | (metric, result) <- metricResultPairs ]
    -- IO [n]
    sequence iOresults

{--
  Takes a list of (InfluxMetrics, n), sends them (via UDP) to a host:port
  returns a list of [n]
-}
sendStats :: String -> Int -> [(InfluxMetrics, n)] -> IO [n]
sendStats host port f = do
    getUDPConnection host port (doClient f)