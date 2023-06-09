{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestInfluxData (influxDataTests) where

import Control.Exception (try)
import Data.Time (zonedTimeToUTC)
import Data.Time.RFC3339 (parseTimeRFC3339)
import F2I.Common (ArchiveStatus (..), InfluxMetric (..))
import F2I.InfluxData (inverter, inverterFromBS, powerFlow, powerFlowFromBS)
import Streaming.Prelude qualified as SP
import Test.HUnit (Test (..), assertBool, assertEqual, assertFailure)

testPowerFlowFromBS :: Test
testPowerFlowFromBS =
    TestLabel "powerflowFromBS - " $
        TestList
            [ TestCase $
                do
                    assertEqual
                        "empty bytstring"
                        (powerFlowFromBS "/dev/null" "")
                        ( ArchiveStatus
                            { path = "/dev/null",
                              realFile = False,
                              success = False,
                              msg = "Error in $: not enough input",
                              metrics = []
                            }
                        ),
              TestCase $
                do
                    assertEqual
                        "empty object"
                        (powerFlowFromBS "/dev/null" "{}")
                        ( ArchiveStatus
                            { path = "/dev/null",
                              realFile = False,
                              success = False,
                              msg = "Error in $: key \"Body\" not found",
                              metrics = []
                            }
                        ),
              TestCase $
                do
                    let
                        timestampUTC = "2023-03-02T20:00:04-00:00" :: String
                        testTimeStamp = zonedTimeToUTC <$> parseTimeRFC3339 timestampUTC

                    assertEqual
                        "empty object"
                        (powerFlowFromBS "/dev/nulls" "{\"Body\":{\"Inverters\":{\"1\":{\"DT\":168,\"E_Day\":1000,\"E_Total\":40580000,\"E_Year\":4000,\"P\":748}},\"Site\":{\"E_Day\":1000,\"E_Total\":40580000,\"E_Year\":4000,\"Meter_Location\":\"unknown\",\"Mode\":\"produce-only\",\"P_Akku\":null,\"P_Grid\":null,\"P_Load\":null,\"P_PV\":748,\"rel_Autonomy\":null,\"rel_SelfConsumption\":null},\"Version\":\"12\"},\"Head\":{\"RequestArguments\":{},\"Status\":{\"Code\":0,\"Reason\":\"\",\"UserMessage\":\"\"},\"Timestamp\":\"2023-03-02T12:00:04-08:00\"}}")
                        ( ArchiveStatus
                            { path = "/dev/nulls",
                              realFile = False,
                              success = True,
                              msg = "",
                              metrics =
                                [ InfluxMetric {measurement = "powerflow", tags = [("version", "12"), ("Meter_Location", "unknown"), ("Mode", "produce-only"), ("id", "site")], field = ("E_Day", Left 1000), timestamp = testTimeStamp},
                                  InfluxMetric {measurement = "powerflow", tags = [("version", "12"), ("Meter_Location", "unknown"), ("Mode", "produce-only"), ("id", "site")], field = ("E_Total", Left 40580000), timestamp = testTimeStamp},
                                  InfluxMetric {measurement = "powerflow", tags = [("version", "12"), ("Meter_Location", "unknown"), ("Mode", "produce-only"), ("id", "site")], field = ("E_Year", Left 4000), timestamp = testTimeStamp},
                                  InfluxMetric {measurement = "powerflow", tags = [("version", "12"), ("Meter_Location", "unknown"), ("Mode", "produce-only"), ("id", "site")], field = ("P_PV", Left 748), timestamp = testTimeStamp},
                                  InfluxMetric {measurement = "powerflow", tags = [("version", "12"), ("id", "1")], field = ("DT", Left 168), timestamp = testTimeStamp},
                                  InfluxMetric {measurement = "powerflow", tags = [("version", "12"), ("id", "1")], field = ("E_Day", Left 1000), timestamp = testTimeStamp},
                                  InfluxMetric {measurement = "powerflow", tags = [("version", "12"), ("id", "1")], field = ("E_Total", Left 40580000), timestamp = testTimeStamp},
                                  InfluxMetric {measurement = "powerflow", tags = [("version", "12"), ("id", "1")], field = ("E_Year", Left 4000), timestamp = testTimeStamp},
                                  InfluxMetric {measurement = "powerflow", tags = [("version", "12"), ("id", "1")], field = ("P", Left 748), timestamp = testTimeStamp}
                                ]
                            }
                        )
            ]

testInverterFromBS :: Test
testInverterFromBS =
    TestLabel "inverterFromBS - " $
        TestList
            [ TestCase $
                do
                    assertEqual
                        "empty bytstring"
                        (inverterFromBS "/dev/null" "")
                        ( ArchiveStatus
                            { path = "/dev/null",
                              realFile = False,
                              success = False,
                              msg = "Error in $: not enough input",
                              metrics = []
                            }
                        ),
              TestCase $
                do
                    assertEqual
                        "empty object"
                        (inverterFromBS "/dev/null" "{}")
                        ( ArchiveStatus
                            { path = "/dev/null",
                              realFile = False,
                              success = False,
                              msg = "Error in $: key \"Body\" not found",
                              metrics = []
                            }
                        ),
              TestCase $
                do
                    let
                        timestampUTC = "2023-03-18T19:00:03-00:00" :: String
                        testTimeStamp = zonedTimeToUTC <$> parseTimeRFC3339 timestampUTC

                    assertEqual
                        "empty object"
                        (inverterFromBS "/dev/nulls" "{\"Body\":{\"DAY_ENERGY\":{\"Unit\":\"Wh\",\"Values\":{\"1\":1000}},\"PAC\":{\"Unit\":\"W\",\"Values\":{\"1\":670}},\"TOTAL_ENERGY\":{\"Unit\":\"Wh\",\"Values\":{\"1\":40622000}},\"YEAR_ENERGY\":{\"Unit\":\"Wh\",\"Values\":{\"1\":46000}}},\"Head\":{\"RequestArguments\":{\"Query\":\"Inverter\",\"Scope\":\"System\"},\"Status\":{\"Code\":0,\"Reason\":\"\",\"UserMessage\":\"\"},\"Timestamp\":\"2023-03-18T12:00:03-07:00\"}}")
                        ( ArchiveStatus
                            { path = "/dev/nulls",
                              realFile = False,
                              success = True,
                              msg = "",
                              metrics =
                                [ InfluxMetric {measurement = "inverter", tags = [("id", "1"), ("unit", "Wh")], field = ("DAY_ENERGY", Left 1000), timestamp = testTimeStamp},
                                  InfluxMetric {measurement = "inverter", tags = [("id", "1"), ("unit", "W")], field = ("PAC", Left 670), timestamp = testTimeStamp},
                                  InfluxMetric {measurement = "inverter", tags = [("id", "1"), ("unit", "Wh")], field = ("TOTAL_ENERGY", Left 40622000), timestamp = testTimeStamp},
                                  InfluxMetric {measurement = "inverter", tags = [("id", "1"), ("unit", "Wh")], field = ("YEAR_ENERGY", Left 46000), timestamp = testTimeStamp}
                                ]
                            }
                        )
            ]

testPowerflow :: Test
testPowerflow =
    TestLabel "powerflow - " $
        TestList
            [ TestCase $ do
                let
                    archiveStatusStream = powerFlow "does-not-exist.powerflow"
                result <- try (SP.length archiveStatusStream)
                case result of
                    Left (_ :: IOError) -> assertBool "We returned an error about an empty file" True
                    Right _ -> assertFailure "missing file should produce error",
              TestCase $ do
                let
                    archiveStatusStream = powerFlow "tests/fixtures/test.powerflow"
                archiveStatusList <- SP.toList_ archiveStatusStream
                archiveStatus : archiveStatusRest <- SP.toList_ archiveStatusStream
                assertEqual "length of stream" 0 $ length archiveStatusRest
                assertBool "file is real" $ realFile archiveStatus
            ]

testInverter :: Test
testInverter =
    TestLabel "inverter - " $
        TestList
            [ TestCase $ do
                let
                    archiveStatusStream = inverter "does-not-exist.inverter"
                result <- try (SP.length archiveStatusStream)
                case result of
                    Left (_ :: IOError) -> assertBool "We returned an error about an empty file" True
                    Right _ -> assertFailure "missing file should produce error",
              TestCase $ do
                let
                    archiveStatusStream = inverter "tests/fixtures/test.inverter"
                archiveStatus : archiveStatusRest <- SP.toList_ archiveStatusStream
                assertEqual "length of stream" 0 $ length archiveStatusRest
                assertBool "file is real" $ realFile archiveStatus
            ]

influxDataTests :: Test
influxDataTests =
    TestList
        [ testInverterFromBS,
          testPowerFlowFromBS,
          testInverter,
          testPowerflow
        ]
