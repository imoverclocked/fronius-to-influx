{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Common(
    ArchiveStatus(..),
    ProcessEntry,
    FroniusToInflux(..),
    InfluxMetric(..),
    defaultArgs
) where

import qualified Data.ByteString.Lazy as BS
import Data.Typeable ( Typeable )
import Data.Data ( Data )
import System.Console.CmdArgs ( (&=), args, details, help, summary, typ, Default(def) )
import Data.Time ( UTCTime )

data InfluxMetric = InfluxMetric {
    measurement :: String,
    tags :: [(String, String)],
    field :: (String, Either Int Bool),
    timestamp :: Maybe UTCTime
} deriving (Show)

data ArchiveStatus = ArchiveStatus {
   path :: String,
   success :: Bool,
   msg :: String,
   metrics :: [InfluxMetric]
} deriving (Show)

type ProcessEntry = String -> BS.ByteString -> IO ArchiveStatus

-- messy but convenient to keep args here so we can just pass the args object around
data FroniusToInflux = FroniusToInflux {
   drop_null :: Bool,
   influx_protocol :: String,
   host :: String,
   port :: Int,
   files :: [FilePath]
   } deriving (Show, Data, Typeable)

defaultArgs :: FroniusToInflux
defaultArgs = FroniusToInflux {
   drop_null = True &= help "Drop null values from influx output data",
   influx_protocol = "udp" &= help "connect via http or udp (default)",
   host = "127.0.0.1" &= help "ignored for stdout connection-type",
   port = 8086,
   files = def &= args &= typ "FILES/DIRS/TAR.GZ"
   } &=
   help "Parse data from a Fronius Data Logger and send it to influxdb" &=
   summary "fronius-to-influx (C) Tim Spriggs" &=
   details [
 --    --------------------------------------------------------------------------------
      "fronius-to-influx takes data logged to an FTP server and converts it to influxdb",
      "for easier digestion of the data (eg: grafana)", "",
      "This tool supports iterating through tar archives that libarchive natively",
      "detects. In practice this brings storage of stats down from 70MB/day down to a",
      "few hundred kB.", "",
      "The tool expects data files to be named as *.powerflow and *.inverter"
   ]
