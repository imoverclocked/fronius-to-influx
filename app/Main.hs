{-# OPTIONS_GHC -fno-cse #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.List ( isSuffixOf )
import System.Console.CmdArgs ( Data, Typeable, (&=), cmdArgs, args, details, help, summary, typ, Default(def) )
import System.Directory ( doesDirectoryExist, listDirectory )
import Common ( ProcessEntry, ArchiveStatus(..), ArchiveStatusStream )
import Archive ( processArchive )
import InfluxData ( powerFlow, powerFlowFromBS, inverter, inverterFromBS )
import InfluxConnection ( sendStats )
import System.Exit ( exitSuccess, die )
import qualified Streaming as S
import qualified Streaming.Prelude as SP
import Data.IORef ( atomicModifyIORef, newIORef, readIORef, IORef )
import Control.Monad ( unless )
import GHC.IO.Handle (hPutStr)
import GHC.IO.StdHandles (stderr)

processDir :: String -> ArchiveStatusStream
processDir path = do
   let paths = S.effect $ _streamLS path
   S.effect $ _flattenStreams $ SP.map processPath paths

_flattenStreams :: SP.Stream (SP.Of ArchiveStatusStream) IO () -> IO ArchiveStatusStream
_flattenStreams sOfS = do
   aOfS <- SP.toList sOfS -- Of [ArchiveStatusStream] ()
   let aOfS' = fst $ SP.lazily aOfS -- [ArchiveStatusStream]
   let s = SP.each aOfS' -- Stream (Of [ArchiveStatusStream]) IO ()
   return $ SP.for s id -- IO ArchiveStatusStream

_streamLS :: String -> IO (SP.Stream (SP.Of FilePath) IO ())
_streamLS path = do
   rawPaths <- listDirectory path
   let paths = map ((path ++ "/") ++) rawPaths
   return $ SP.each paths

processArchiveOrDir :: String -> ArchiveStatusStream
processArchiveOrDir path = S.effect $ _archiveOrDir path

_archiveOrDir :: String -> IO ArchiveStatusStream
_archiveOrDir path = do
   isDirS <- doesDirectoryExist path
   if isDirS
      then return $ processDir path
      else return $ processArchive path processPathFromBS

processPath :: String -> ArchiveStatusStream
processPath path
   | ".powerflow" `isSuffixOf` path = powerFlow path
   | ".inverter"  `isSuffixOf` path = inverter path
   | otherwise = processArchiveOrDir path

processPathFromBS :: ProcessEntry
processPathFromBS path bs
   |  ".powerflow" `isSuffixOf` path = powerFlowFromBS path bs
   |  ".inverter"  `isSuffixOf` path = inverterFromBS path bs
   | otherwise = ArchiveStatus{
      path = path,
      success = False,
      msg = "Embedded archives are not supported",
      metrics = []
      }

_incSuccessFail :: IORef Int -> IORef Int -> ArchiveStatus -> IO ()
_incSuccessFail successC failC archiveStatus = if success archiveStatus
   then atomicModifyIORef successC increment
   else atomicModifyIORef failC increment
   where increment a = (a+1, ())

_recordFailedEntries :: IORef [ArchiveStatus] -> ArchiveStatus -> IO ()
_recordFailedEntries archiveStatusList archiveStatus = unless (success archiveStatus)
   (atomicModifyIORef archiveStatusList (appendS archiveStatus))
   where appendS a b = (b ++ [a], ())

data FroniusToInflux = FroniusToInflux {
   influx_protocol :: String,
   host :: String,
   port :: Int,
   files :: [FilePath]
   } deriving (Show, Data, Typeable)

defaultArgs :: FroniusToInflux
defaultArgs = FroniusToInflux {
   influx_protocol = "udp" &= help "connect via http, https or udp (default)",
   host = "127.0.0.1" &= help "default: 127.0.0.1",
   port = 8086 &= help "default: 8086",
   files = def &= args &= typ "FILES/DIRS/TAR.GZ/TAR.XZ"
   } &=
   help "Parse data from a Fronius Data Logger and send it to influxdb" &=
   summary "fronius-to-influx (C) Tim Spriggs" &=
   details [
 --    --------------------------------------------------------------------------------
      "fronius-to-influx takes data logged to an FTP server and converts it to ",
      "influxdb for easier digestion of the data (eg: grafana)", "",
      "This tool supports iterating through tar archives that libarchive natively",
      "detects. In practice this brings storage of stats down from 70MB/day down to a",
      "few hundred kB.", "",
      "The tool expects data files to be named as *.powerflow and *.inverter"
   ]

main :: IO ()
main = do
   -- arg processing
   realArgs <- cmdArgs defaultArgs

   let pathStatusRaw = map processPath $ files realArgs
   pathStatus <- _flattenStreams $ SP.each pathStatusRaw

   -- Keep track of some stats and failed entries for the user's benefit
   successCount <- newIORef (0 :: Int)
   failCount <- newIORef (0 :: Int)
   failedEntries <- newIORef ([] :: [ArchiveStatus])

   let pathStatus' =
         SP.chain (_incSuccessFail successCount failCount) $
         SP.chain (_recordFailedEntries failedEntries) pathStatus

   results <- SP.toList . SP.filter success $ pathStatus'
   let archiveList = fst $ SP.lazily results

   sendStats (influx_protocol realArgs) (host realArgs) (port realArgs) archiveList

   finalSuccessCount <- readIORef successCount
   finalFailCount <- readIORef failCount
   let totalCount = finalSuccessCount + finalFailCount

   -- output to stderr so we can implement a stdout "protocol" in the future
   hPutStr stderr $ "Processed " ++ show finalSuccessCount ++ "/" ++ show totalCount ++ " successfully.\n"

   if finalFailCount > 0
      then do
         finalFailedEntris <- readIORef failedEntries
         die $ show finalFailedEntris
      else exitSuccess
