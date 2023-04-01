{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
{-# OPTIONS_GHC -Wno-unsafe #-} -- because networking
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe #-} 

module InfluxConnection(
    sendStats
) where

import Prelude (String, Int, IO, Maybe (Nothing, Just), Foldable (foldr, foldMap), Either (Left, Right), Bool (False, True), map, ($), (++), fromIntegral, (.), Show (show))
import Common ( ArchiveStatus(metrics, success), InfluxMetric(measurement, tags, field, timestamp) )
import Network.Socket ( SockAddr, Socket )
import Network.Run.UDP ( runUDPClient )

import Database.InfluxDB ( Line(Line), Field(FieldBool, FieldInt), HasServer (server) )
import Database.InfluxDB.Types ( Measurement, Key, LineField, Server(Server, _host, _port, _ssl) )
import Database.InfluxDB.Write qualified as HTTP
import Database.InfluxDB.Write.UDP qualified as UDP
import Data.String ( IsString(fromString) )
import Data.Map ( Map, fromList )
import System.Exit (die)
import Control.Lens ((.~), (&))
import Data.Time (UTCTime)

getUDPConnection :: String -> Int -> (Socket -> SockAddr -> IO m) -> IO m
getUDPConnection host port = runUDPClient host (show port)

_archiveStatusToMetric :: ArchiveStatus -> Maybe ArchiveStatus
_archiveStatusToMetric aS = if success aS
    then Just aS
    else Nothing

_writeUDPMetrics :: (Foldable f) => f ArchiveStatus -> UDP.WriteParams -> IO ()
_writeUDPMetrics archiveStatusF params =
    -- write the metrics (potentially into the UDP void)
    foldMap (_writeUDPMetricsF params . metrics) archiveStatusF

    -- Not inlinable because GHC.Base.$fMonoidIO is not inlinable

    -- Can't do this because https://github.com/maoe/influxdb-haskell/issues/92
    -- UDP.writeBatch params metricList

_writeUDPMetricsF :: (Foldable f) => UDP.WriteParams -> f InfluxMetric -> IO ()
_writeUDPMetricsF params = foldMap (UDP.write params . _lineFromMetric)

doUDPClient :: (Foldable f) => f ArchiveStatus -> Socket -> SockAddr -> IO ()
doUDPClient archiveStatusList sock sockAddr = do
    let params = UDP.writeParams sock sockAddr
    _writeUDPMetrics archiveStatusList params

_accumulateMetrics :: ArchiveStatus -> [InfluxMetric] -> [InfluxMetric]
_accumulateMetrics archiveStatus extractedMetrics = extractedMetrics ++ metrics archiveStatus

_lineFromMetric :: InfluxMetric -> Line UTCTime
_lineFromMetric metric = do
    -- Type wrangling
    let m = fromString (measurement metric) :: Measurement
    let t = Data.Map.fromList [ (fromString k, fromString v) | (k, v) <- tags metric ]
    let f = Data.Map.fromList [ case field metric of
                                    (k, Left s) -> (fromString k, FieldInt $ fromIntegral s)
                                    (k, Right b) -> (fromString k, FieldBool b)
                                ] :: Map Key LineField
    let ts = timestamp metric

    Line m t f ts

writeHTTPMetrics :: (Foldable f) => f ArchiveStatus -> HTTP.WriteParams -> IO ()
writeHTTPMetrics archiveStatusF params =
    HTTP.writeBatch params $
        map _lineFromMetric $
        foldr _accumulateMetrics [] archiveStatusF

doHTTPClient :: (Foldable f) => String -> Int -> Bool -> f ArchiveStatus -> IO ()
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
sendStats :: (Foldable f) => String -> String -> Int -> f ArchiveStatus -> IO ()
sendStats "udp" host port s = getUDPConnection host port (doUDPClient s)
sendStats "http" host port s = doHTTPClient host port False s
sendStats "https" host port s = doHTTPClient host port True s
sendStats unknown _ _ _ = die $ "Unknown protocol: " ++ unknown

-- sendStats is not inlinable, see note above
