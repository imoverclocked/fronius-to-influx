{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Unsafe #-}

module FroniusData(
    HeadData(..),
    InverterStat(..),
    InverterEntry(..),
    PowerflowBody(..),
    PowerflowEntry(..)
) where

import FroniusCommon        ( HeadData(..) )
import FroniusPowerflowData ( PowerflowBody(..), PowerflowEntry(..) )
import FroniusInverterData  ( InverterEntry(..), InverterStat(..) )
