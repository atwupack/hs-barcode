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
    -- * Common stuff
    Bar(..),encode, save, Dimensions(..),
    -- * Supported linear barcodes
    -- ** Code 11
    Code11(..), CheckDigit(..),
    -- ** Code 39
    Code39(..),
    -- ** Code 93
    Code93(..)
) where

import Barcode.Linear.Code11
import Barcode.Linear.Code39
import Barcode.Linear.Code93
import Barcode.Linear.Common
import Barcode.Linear.Output

saveEnc :: Maybe [Bar] -> Dimensions -> FilePath -> IO()
saveEnc Nothing _ _ = print "Error"
saveEnc (Just a) dims path = saveBarcode dims a path

save :: (Encoder a) => a -> String -> Dimensions -> FilePath -> IO()
save enc text =
    saveEnc (encode enc text)

