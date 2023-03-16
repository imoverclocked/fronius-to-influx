{-# OPTIONS_GHC -fno-cse #-}

module Main where

import Data.List (isSuffixOf)
import System.Console.CmdArgs
import System.Directory
import Common
import Archive ( processArchive )
import InfluxData
import InfluxConnection
import System.Exit (exitSuccess, die)

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

   _ <- sendStats
      (influx_protocol realArgs)
      (host realArgs)
      (port realArgs)
      [(metrics status, status) | status <- pathStatus]

   let successCount = length $ [i | i <- pathStatus, success i]
   let failureCount = length $ [i | i <- pathStatus, not (success i)]

   putStrLn $ "Processed " ++ (show successCount) ++ "/" ++ (show $ length pathStatus) ++ " successfully."
   if failureCount > 0
      then do
         die $ show [pS | pS <- pathStatus, not $ success pS]
      else exitSuccess
