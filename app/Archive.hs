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
import qualified Streaming as S
import qualified Streaming.Prelude as SP

type Decompressor = BSL.ByteString -> BSL.ByteString

getDecompressor :: String -> Decompressor
getDecompressor path
    | ".gz"   `isSuffixOf` path = GZip.decompress
    | ".tgz"  `isSuffixOf` path = GZip.decompress
    | ".txz"  `isSuffixOf` path = Lzma.decompress
    | ".xz"  `isSuffixOf` path = Lzma.decompress
    | ".Z"  `isSuffixOf` path = Zlib.decompress
    | otherwise = id

processArchive :: String -> ProcessEntry -> ArchiveStatusStream
processArchive path entryProcessor = S.effect $ _processArchive path entryProcessor

_processArchive :: String -> ProcessEntry -> IO ArchiveStatusStream
_processArchive path entryProcessor = do
    let decompressor = getDecompressor path
    archiveBSL <- BSL.readFile path
    let archiveDecodedBSL = decompressor archiveBSL
    case Archive.FFI.readArchiveBytes archiveDecodedBSL of
        Left err -> return $ SP.yield ArchiveStatus{
            path = path,
            success = False,
            msg = show err ++ ": Perhaps an unsupported compression format?",
            metrics = []
            }
        -- TODO: plumb Streaming API into processArchiveContents
        Right entries -> return $ SP.each $ processArchiveContents entryProcessor entries

entryToArchiveStatus :: ProcessEntry -> String -> EntryContent String BS.ByteString -> ArchiveStatus
entryToArchiveStatus entryProcessor filepath content =
    case content of
        NormalFile e -> entryProcessor filepath e
        _ -> do ArchiveStatus{
            path = filepath,
            success = True,
            msg = "skipped non-file",
            metrics = []
            }

processArchiveContents :: ProcessEntry -> [Archive.FFI.Entry] -> [ArchiveStatus]
processArchiveContents entryProcessor entries = [entryToArchiveStatus entryProcessor (filepath e) (content e) | e <- entries]
