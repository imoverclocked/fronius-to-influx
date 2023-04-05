{-# LANGUAGE OverloadedStrings #-}

module TestFroniusData (froniusDataTests) where

import Data.Aeson (ToJSON (..), decode, encode)
import F2I.FroniusData
import Test.HUnit (Test (..), assertEqual, assertFailure)

sampleInverterStrings =
    [ "{\"Body\":{\"DAY_ENERGY\":{\"Unit\":\"Wh\",\"Values\":{\"1\":631}},\"PAC\":{\"Unit\":\"W\",\"Values\":{\"1\":0}},\"TOTAL_ENERGY\":{\"Unit\":\"Wh\",\"Values\":{\"1\":40602000}},\"YEAR_ENERGY\":{\"Unit\":\"Wh\",\"Values\":{\"1\":26000}}},\"Head\":{\"RequestArguments\":{\"Query\":\"Inverter\",\"Scope\":\"System\"},\"Status\":{\"Code\":0,\"Reason\":\"\",\"UserMessage\":\"\"},\"Timestamp\":\"2023-03-11T07:37:23-08:00\"}}",
      "{\"Body\":{\"DAY_ENERGY\":{\"Unit\":\"Wh\",\"Values\":{\"1\":0}},\"PAC\":{\"Unit\":\"W\",\"Values\":{}},\"TOTAL_ENERGY\":{\"Unit\":\"Wh\",\"Values\":{\"1\":0}},\"YEAR_ENERGY\":{\"Unit\":\"Wh\",\"Values\":{\"1\":0}}},\"Head\":{\"RequestArguments\":{\"Query\":\"Inverter\",\"Scope\":\"System\"},\"Status\":{\"Code\":0,\"Reason\":\"\",\"UserMessage\":\"\"},\"Timestamp\":\"2023-03-11T23:59:55-08:00\"}}"
    ]

sampleHeadStrings =
    [ "{\"RequestArguments\":{\"Query\":\"Inverter\",\"Scope\":\"System\"},\"Status\":{\"Code\":0,\"Reason\":\"\",\"UserMessage\":\"\"},\"Timestamp\":\"2023-03-11T07:37:23-08:00\"}"
    ]

sampleInverterStatStrings =
    [ "{\"Unit\":\"Wh\",\"Values\":{\"1\":631}}",
      "{\"Unit\":\"W\",\"Values\":{\"1\":0}}",
      "{\"Unit\":\"Wh\",\"Values\":{\"1\":40602000}}",
      "{\"Unit\":\"Wh\",\"Values\":{\"1\":26000}}"
    ]

samplePowerflowStrings =
    [ "{\"Body\":{\"Inverters\":{\"1\":{\"DT\":168,\"E_Day\":1000,\"E_Total\":40603000,\"E_Year\":27000,\"P\":428}},\"Site\":{\"E_Day\":1000,\"E_Total\":40603000,\"E_Year\":27000,\"Meter_Location\":\"unknown\",\"Mode\":\"produce-only\",\"P_Akku\":null,\"P_Grid\":null,\"P_Load\":null,\"P_PV\":428,\"rel_Autonomy\":null,\"rel_SelfConsumption\":null},\"Version\":\"12\"},\"Head\":{\"RequestArguments\":{},\"Status\":{\"Code\":0,\"Reason\":\"\",\"UserMessage\":\"\"},\"Timestamp\":\"2023-03-11T11:59:45-08:00\"}}"
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

froniusDataTests :: Test
froniusDataTests =
    TestList
        [ TestList $
            map
                (\a -> TestLabel "InverterStat decode/encode" $ encdenc (decode a :: Maybe F2I.FroniusData.InverterStat) a)
                sampleInverterStatStrings,
          TestList $
            map
                (\a -> TestLabel "HeadData decode/encode" $ encdenc (decode a :: Maybe F2I.FroniusData.HeadData) a)
                sampleHeadStrings,
          TestList $
            map
                (\a -> TestLabel "InverterEntry decode/encode" $ encdenc (decode a :: Maybe F2I.FroniusData.InverterEntry) a)
                sampleInverterStrings,
          TestList $
            map
                (\a -> TestLabel "Powerflow decode/encode" $ encdenc (decode a :: Maybe F2I.FroniusData.PowerflowEntry) a)
                samplePowerflowStrings
        ]
