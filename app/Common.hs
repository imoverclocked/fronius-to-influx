module Common(
    ArchiveStatus(..),
    ProcessEntry(..)
) where

import qualified Data.ByteString as BS

data ArchiveStatus = ArchiveStatus {
   path :: String,
   success :: Bool,
   msg :: String
} deriving (Show)

type ProcessEntry = String -> BS.ByteString -> IO ArchiveStatus
