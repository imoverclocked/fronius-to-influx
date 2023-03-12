module InfluxData(
    powerFlow,
    powerFlowFromBS,
    inverter,
    inverterFromBS
) where

import Common
import qualified Data.ByteString as BS


powerFlow :: String -> IO ArchiveStatus
powerFlow path = do
    bits <- BS.readFile path
    powerFlowFromBS path bits

powerFlowFromBS :: String -> BS.ByteString -> IO ArchiveStatus
powerFlowFromBS path content = do
   return ArchiveStatus{path = path, success = True, msg = "TODO: Process powerflow data: " ++ show (BS.length content)}

inverter :: String -> IO ArchiveStatus
inverter path = do
    bits <- BS.readFile path
    inverterFromBS path bits

inverterFromBS :: String -> BS.ByteString -> IO ArchiveStatus
inverterFromBS path content = do
   return ArchiveStatus{path = path, success = True, msg = "TODO: Process inverter data: " ++ show (BS.length content)}
