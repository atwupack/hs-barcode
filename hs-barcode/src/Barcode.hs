-----------------------------------------------------------------------------
--
-- Module      :  Barcode
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

module Barcode (
    BarcodeError(..)
) where

-- | Type representing the errors of this library.
data BarcodeError
    = IllegalCharacter Char -- ^ A given character is not supported by a type of barcode.


