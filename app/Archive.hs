{-# LANGUAGE OverloadedStrings #-}

module Archive where

import Common

import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.Zlib as Zlib
import Data.List (isSuffixOf)

import Archive.FFI (readArchiveBytes, Entry)
import Codec.Archive
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

type Decompressor = BSL.ByteString -> BSL.ByteString

getDecompressor :: String -> Decompressor
getDecompressor path
    | ".gz"   `isSuffixOf` path = GZip.decompress
    | ".tgz"  `isSuffixOf` path = GZip.decompress
    | ".Z"  `isSuffixOf` path = Zlib.decompress
    | otherwise = id

processArchive :: String -> ProcessEntry -> IO [ArchiveStatus]
processArchive path entryProcessor = do
    archiveBSL <- BSL.readFile path
    let archiveDecodedBSL = getDecompressor path archiveBSL
    case Archive.FFI.readArchiveBytes archiveDecodedBSL of
        Left err -> return [ArchiveStatus{
            path = path,
            success = False,
            msg = show err ++ ": Perhaps an unsupported compression format?"}]
        Right entries -> processArchiveContents entryProcessor entries

entryToArchiveStatus :: ProcessEntry -> String -> EntryContent String BS.ByteString -> IO ArchiveStatus
entryToArchiveStatus entryProcessor filepath content =
    case content of
        NormalFile e -> entryProcessor filepath e
        _ -> do return ArchiveStatus{path = filepath, success = True, msg = "skipped non-file"}

processArchiveContents :: ProcessEntry -> [Archive.FFI.Entry] -> IO [ArchiveStatus]
processArchiveContents entryProcessor entries = sequence [entryToArchiveStatus entryProcessor (filepath e) (content e) | e <- entries]