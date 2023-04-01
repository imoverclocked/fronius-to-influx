{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe #-}

module Archive(
    Decompressor,
    getDecompressor,
    processArchive,
) where

import Prelude
    (
        String,
        IO,
        Either (Left, Right),
        Monad (return),
        Show (show),
        Bool (True, False),
        ($),
        otherwise,
        id
    )
import Common ( ProcessEntry, ArchiveStatus (ArchiveStatus, path, realFile, success, msg, metrics), ArchiveStatusStream )

import Codec.Compression.GZip qualified as GZip
import Codec.Compression.Lzma qualified as Lzma
import Codec.Compression.Zlib qualified as Zlib
import Data.List (isSuffixOf, (++))

import Archive.FFI (readArchiveBytes, Entry)
import Codec.Archive ( Entry(content, filepath), EntryContent(NormalFile) )
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Streaming qualified as S
import Streaming.Prelude qualified as SP
import Data.Kind (Type)

type Decompressor :: Type
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
    let baseArchiveStatus = ArchiveStatus{
            path = path,
            realFile = True,
            success = False,
            msg = "file based archive",
            metrics = []
    }
    case Archive.FFI.readArchiveBytes archiveDecodedBSL of
        Left err -> return $ SP.yield baseArchiveStatus {
            msg = show err ++ ": Perhaps an unsupported compression format?"
            }
        Right entries -> return $ SP.each $
            baseArchiveStatus {success = True} : processArchiveContents entryProcessor entries

entryToArchiveStatus :: ProcessEntry -> String -> EntryContent String BS.ByteString -> ArchiveStatus
entryToArchiveStatus entryProcessor filepath content =
    case content of
        NormalFile e -> entryProcessor filepath e
        _ -> ArchiveStatus{
            path = filepath,
            realFile = False,
            success = True,
            msg = "skipped non-file",
            metrics = []
            }

processArchiveContents :: ProcessEntry -> [Archive.FFI.Entry] -> [ArchiveStatus]
processArchiveContents entryProcessor entries = [entryToArchiveStatus entryProcessor (filepath e) (content e) | e <- entries]
