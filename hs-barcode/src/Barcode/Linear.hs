-----------------------------------------------------------------------------
--
-- Module      :  Barcode.Linear
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Barcode.Linear (
    Bar(..)
) where

import Data.Char

data Bar = Black Int | White Int

instance Show Bar where
   show (Black w) = intToDigit w : "B"
   show (White w) = intToDigit w : "W"

