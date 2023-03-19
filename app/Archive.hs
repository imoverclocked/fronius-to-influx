{-# LANGUAGE OverloadedStrings #-}

module Archive where

import Common

import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.Lzma as Lzma
import qualified Codec.Compression.Zlib as Zlib
import Data.List (isSuffixOf)

import Archive.FFI (readArchiveBytes, Entry)
import Codec.Archive ( Entry(content, filepath), EntryContent(NormalFile) )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

type Decompressor = BSL.ByteString -> BSL.ByteString

getDecompressor :: String -> Decompressor
getDecompressor path
    | ".gz"   `isSuffixOf` path = GZip.decompress
    | ".tgz"  `isSuffixOf` path = GZip.decompress
    | ".txz"  `isSuffixOf` path = Lzma.decompress
    | ".xz"  `isSuffixOf` path = Lzma.decompress
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
            msg = show err ++ ": Perhaps an unsupported compression format?",
            metrics = []
            }]
        Right entries -> return $ processArchiveContents entryProcessor entries

entryToArchiveStatus :: ProcessEntry -> String -> EntryContent String BS.ByteString -> ArchiveStatus
entryToArchiveStatus entryProcessor filepath content =
    case content of
        NormalFile e -> entryProcessor filepath $ BSL.fromStrict e
        _ -> do ArchiveStatus{
            path = filepath,
            success = True,
            msg = "skipped non-file",
            metrics = []
            }

processArchiveContents :: ProcessEntry -> [Archive.FFI.Entry] -> [ArchiveStatus]
processArchiveContents entryProcessor entries = [entryToArchiveStatus entryProcessor (filepath e) (content e) | e <- entries]
