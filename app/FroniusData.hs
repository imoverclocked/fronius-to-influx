{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module FroniusData (
    HeadData (..),
    InverterStat (..),
    InverterEntry (..),
    PowerflowBody (..),
    PowerflowEntry (..),
) where

import FroniusCommon (HeadData (..))
import FroniusInverterData (InverterEntry (..), InverterStat (..))
import FroniusPowerflowData (PowerflowBody (..), PowerflowEntry (..))
