{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import Data.List (isSuffixOf)
import System.Console.CmdArgs
import System.Directory
import Common
import Archive ( processArchive )
import InfluxData
import System.Exit (exitSuccess, exitFailure)

data FroniusToInflux = FroniusToInflux {
   drop_null :: Bool,
   files :: [FilePath]
   } deriving (Show, Data, Typeable)

defaultArgs :: FroniusToInflux
defaultArgs = FroniusToInflux {
   drop_null = True &= help "Drop null values from influx output data",
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

processDir :: String -> IO [ArchiveStatus]
processDir path = do
   rawPaths <- listDirectory path
   let paths = map ((path ++ "/") ++) rawPaths
   processedPaths <- mapM processPath paths
   return (concat processedPaths)

processArchiveOrDir :: String -> IO [ArchiveStatus]
processArchiveOrDir path = do
   isDir <- doesDirectoryExist path
   if isDir
      then processDir path
      else processArchive path processPathFromBS

processPath :: String -> IO [ArchiveStatus]
processPath path
   |  ".powerflow" `isSuffixOf` path = do
         status <- powerFlow path
         return [status]
   |  ".inverter"  `isSuffixOf` path = do
         status <- inverter path
         return [status]
   | otherwise = processArchiveOrDir path

processPathFromBS :: ProcessEntry
processPathFromBS path bs
   |  ".powerflow" `isSuffixOf` path = powerFlowFromBS path bs
   |  ".inverter"  `isSuffixOf` path = inverterFromBS path bs
   | otherwise = return ArchiveStatus{path = path, success = False, msg = "Embedded archives are not supported"}

main :: IO ()
main = do
   realArgs <- cmdArgs defaultArgs
   pathStatusRaw <- mapM processPath $ files realArgs
   let pathStatus = concat pathStatusRaw
   let successCount = length $ [i | i <- pathStatus, success i]
   let failureCount = length $ [i | i <- pathStatus, not (success i)]
   putStrLn $ "Processed " ++ (show successCount) ++ "/" ++ (show $ length pathStatus) ++ " successfully."
   if failureCount > 0
      then do
         print [pS | pS <- pathStatus, not $ success pS]
         exitFailure
      else exitSuccess
