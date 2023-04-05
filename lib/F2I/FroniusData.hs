{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module F2I.FroniusData (
    HeadData (..),
    InverterStat (..),
    InverterEntry (..),
    PowerflowBody (..),
    PowerflowEntry (..),
) where

import F2I.FroniusCommon (HeadData (..))
import F2I.FroniusInverterData (InverterEntry (..), InverterStat (..))
import F2I.FroniusPowerflowData (PowerflowBody (..), PowerflowEntry (..))
