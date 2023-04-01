{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
{-# OPTIONS_GHC -Wno-unsafe #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main (main) where

import Archive (processArchive)
import Common (ArchiveStatus (ArchiveStatus, metrics, msg, path, realFile, success), ArchiveStatusStream, ProcessEntry)
import Control.Monad (unless, when)
import Data.IORef (IORef, atomicModifyIORef, newIORef, readIORef)
import Data.Kind (Type)
import Data.List (isInfixOf, isSuffixOf)
import GHC.IO.Handle (hPutStr)
import GHC.IO.StdHandles (stderr)
import InfluxConnection (sendStats)
import InfluxData (inverter, inverterFromBS, powerFlow, powerFlowFromBS)
import Streaming qualified as S
import Streaming.Prelude qualified as SP
import System.Console.CmdArgs (Data, Default (def), args, cmdArgs, details, help, summary, typ, (&=))
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory, makeRelativeToCurrentDirectory, removeDirectory, renameFile)
import System.Exit (die, exitSuccess)
import System.FilePath (dropFileName, pathSeparator)
import Prelude (
    Bool (False, True),
    FilePath,
    IO,
    Int,
    Ord ((>)),
    Show,
    String,
    filter,
    fst,
    id,
    map,
    mapM,
    mapM_,
    not,
    null,
    otherwise,
    return,
    show,
    ($),
    (&&),
    (+),
    (++),
    (.),
 )

processDir :: String -> ArchiveStatusStream
processDir path = do
    let
        paths = S.effect $ _streamLS path
    S.effect $ _flattenStreams $ SP.map processPath paths

_flattenStreams :: SP.Stream (SP.Of ArchiveStatusStream) IO () -> IO ArchiveStatusStream
_flattenStreams sOfS = do
    aOfS <- SP.toList sOfS :: IO (SP.Of [ArchiveStatusStream] ())
    let
        aOfS' = fst $ SP.lazily aOfS :: [ArchiveStatusStream]
    let
        s = SP.each aOfS' -- :: SP.Stream (SP.Of [ArchiveStatusStream]) IO ()
    return $ SP.for s id

_streamLS :: String -> IO (SP.Stream (SP.Of FilePath) IO ())
_streamLS path = do
    rawPaths <- listDirectory path
    let
        paths = map ((path ++ "/") ++) rawPaths
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
    | ".inverter" `isSuffixOf` path = inverter path
    | otherwise = processArchiveOrDir path

processPathFromBS :: ProcessEntry
processPathFromBS path bs
    | ".powerflow" `isSuffixOf` path = powerFlowFromBS path bs
    | ".inverter" `isSuffixOf` path = inverterFromBS path bs
    | otherwise =
        ArchiveStatus
            { path = path,
              realFile = False,
              success = False,
              msg = "Embedded archives are not supported",
              metrics = []
            }

_incSuccessFail :: IORef Int -> IORef Int -> ArchiveStatus -> IO ()
_incSuccessFail successC failC archiveStatus =
    if success archiveStatus
        then atomicModifyIORef successC incrementA
        else atomicModifyIORef failC incrementA
  where
    incrementA a = (a + 1, ())

_recordFailedEntries :: IORef [ArchiveStatus] -> ArchiveStatus -> IO ()
_recordFailedEntries archiveStatusList archiveStatus =
    unless
        (success archiveStatus)
        (atomicModifyIORef archiveStatusList (appendS archiveStatus))
  where
    appendS a b = (b ++ [a], ())

_recordProcessedPaths :: IORef [FilePath] -> ArchiveStatus -> IO ()
_recordProcessedPaths processedPaths archiveStatus =
    when (success archiveStatus && realFile archiveStatus) $
        atomicModifyIORef processedPaths $
            appendS (path archiveStatus)
  where
    appendS path paths = (path : paths, ())

_moveProcessedFiles :: FilePath -> [FilePath] -> IO ()
_moveProcessedFiles destinationProcessedDir finalProcessedPaths =
    if not . null $ destinationProcessedDir
        then do
            relativePaths <- mapM makeRelativeToCurrentDirectory finalProcessedPaths
            let
                unconformingPaths = filter (".." `isInfixOf`) relativePaths
            unless (null unconformingPaths) $
                die $
                    "File paths with indirections can not be renamed: " ++ show unconformingPaths

            -- create destination directory structure
            mapM_
                ( (createDirectoryIfMissing True . (\path -> destinationProcessedDir ++ [pathSeparator] ++ path)) . dropFileName
                )
                relativePaths

            -- move successfully processed archives to destiation
            mapM_
                (\path -> renameFile path (destinationProcessedDir ++ "/" ++ path))
                relativePaths

            -- clean up empty archive directories (admittedly naive implementation that may miss nested edge cases)
            mapM_
                ( ( \path -> do
                        srcDir <- listDirectory path
                        when (null srcDir) $ removeDirectory path
                  )
                    . dropFileName
                )
                relativePaths
            hPutStr stderr $ "Moved processed paths (dest: " ++ destinationProcessedDir ++ "): " ++ show relativePaths ++ "\n"
        else hPutStr stderr "Leaving files in place.\n"

type FroniusToInflux :: Type
data FroniusToInflux = FroniusToInflux
    { influx_protocol :: String,
      host :: String,
      port :: Int,
      processed :: FilePath,
      files :: [FilePath]
    }
    deriving stock (Show, Data)

defaultArgs :: FroniusToInflux
defaultArgs =
    FroniusToInflux
        { influx_protocol = "udp" &= help "connect via http, https or udp (default)",
          host = "127.0.0.1" &= help "default: 127.0.0.1",
          port = 8086 &= help "default: 8086",
          processed = "" &= help "destination for processed files (preserves relative paths from current directory)",
          files = def &= args &= typ "FILES/DIRS/TAR.GZ/TAR.XZ"
        }
        &= help "Parse data from a Fronius Data Logger and send it to influxdb"
        &= summary "fronius-to-influx (C) Tim Spriggs"
        &= details
            [ --    --------------------------------------------------------------------------------
              "fronius-to-influx takes data logged to an FTP server and converts it to ",
              "influxdb for easier digestion of the data (eg: grafana)",
              "",
              "This tool supports iterating through tar archives that libarchive natively",
              "detects. In practice this brings storage of stats down from 70MB/day down to a",
              "few hundred kB.",
              "",
              "The tool expects data files to be named as *.powerflow and *.inverter"
            ]

main :: IO ()
main = do
    -- arg processing
    realArgs <- cmdArgs defaultArgs

    let
        pathStatusRaw = map processPath $ files realArgs
    pathStatus <- _flattenStreams $ SP.each pathStatusRaw

    -- Keep track of some stats and failed entries for the user's benefit
    successCount <- newIORef (0 :: Int)
    failCount <- newIORef (0 :: Int)
    processedPaths <- newIORef ([] :: [FilePath])
    failedEntries <- newIORef ([] :: [ArchiveStatus])

    let
        pathStatus' =
            SP.chain (_recordProcessedPaths processedPaths) $
                SP.chain (_incSuccessFail successCount failCount) $
                    SP.chain (_recordFailedEntries failedEntries) pathStatus

    results <- SP.toList . SP.filter success $ pathStatus'
    let
        archiveList = fst $ SP.lazily results

    sendStats (influx_protocol realArgs) (host realArgs) (port realArgs) archiveList

    finalSuccessCount <- readIORef successCount
    finalFailCount <- readIORef failCount
    let
        totalCount = finalSuccessCount + finalFailCount

    finalProcessedPaths <- readIORef processedPaths
    let
        destinationProcessedDir = processed realArgs
    _moveProcessedFiles destinationProcessedDir finalProcessedPaths

    -- output to stderr so we can implement a stdout "protocol" in the future
    hPutStr stderr $ "Processed " ++ show finalSuccessCount ++ "/" ++ show totalCount ++ " successfully.\n"

    if finalFailCount > 0
        then do
            finalFailedEntris <- readIORef failedEntries
            die $ show finalFailedEntris
        else exitSuccess
