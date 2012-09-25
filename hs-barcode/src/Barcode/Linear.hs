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
    Bar(..),encode, save, Dimensions(..),BarcodeError(..),
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
import Barcode.Error
import Control.Monad.Error


save :: (Encoder a) => a -> String -> Dimensions -> FilePath -> ErrorT BarcodeError IO()
save enc text dim path = do
    bar <- encode enc text
    liftIO $ saveBarcode dim bar path
