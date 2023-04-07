{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module TestFroniusAPIData (froniusAPIDataTests) where

import Data.Aeson (ToJSON (..), decode, encode)
import F2I.FroniusData qualified as FD
import Test.HUnit (Test (..), assertEqual, assertFailure)

sampleInverterAPIStrings =
    [ "{\"Body\":{\"Data\":{\"1\":{\"CustomName\":\"\",\"DT\":168,\"ErrorCode\":0,\"PVPower\":3800,\"Show\":1,\"StatusCode\":-1,\"UniqueID\":\"195714\"}}},\"Head\":{\"RequestArguments\":{},\"Status\":{\"Code\":0,\"Reason\":\"\",\"UserMessage\":\"\"},\"Timestamp\":\"2023-04-06T21:43:53-07:00\"}}"
    ]

samplePowerflowAPIStrings =
    [ "{\"Body\":{\"Data\":{\"Inverters\":{\"1\":{\"DT\":168,\"E_Day\":7000,\"E_Total\":40709000,\"E_Year\":133000,\"P\":0}},\"Site\":{\"E_Day\":7000,\"E_Total\":40709000,\"E_Year\":133000,\"Meter_Location\":\"unknown\",\"Mode\":\"produce-only\",\"P_Akku\":null,\"P_Grid\":null,\"P_Load\":null,\"P_PV\":null,\"rel_Autonomy\":null,\"rel_SelfConsumption\":null},\"Version\":\"12\"}},\"Head\":{\"RequestArguments\":{},\"Status\":{\"Code\":0,\"Reason\":\"\",\"UserMessage\":\"\"},\"Timestamp\":\"2023-04-06T21:43:51-07:00\"}}"
    ]

-- Test that encode/decode produces the same result
encdenc :: (ToJSON a, Show b) => Maybe a -> b -> Test
encdenc Nothing sourceStr = TestCase (assertFailure $ "No object parsed from " ++ show sourceStr)
encdenc (Just decodedObj) sourceStr =
    TestLabel "decode->encode produces same result" $
        TestCase
            ( assertEqual
                "json decode+encode produces same result"
                (show sourceStr)
                (show $ encode decodedObj)
            )

froniusAPIDataTests :: Test
froniusAPIDataTests =
    TestList
        [ TestList $
            map
                (\a -> TestLabel "InverterEntry decode/encode" $ encdenc (decode a :: Maybe FD.InverterAPIEntry) a)
                sampleInverterAPIStrings,
          TestList $
            map
                (\a -> TestLabel "Powerflow decode/encode" $ encdenc (decode a :: Maybe FD.PowerflowAPIEntry) a)
                samplePowerflowAPIStrings
        ]
