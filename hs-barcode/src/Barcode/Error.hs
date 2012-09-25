-----------------------------------------------------------------------------
--
-- Module      :  Barcode.Error
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

module Barcode.Error (
    BarcodeError(..), (<?>)
) where

import Control.Monad.Error

-- | Type representing the errors of this library.
data BarcodeError
    -- | A given character is not supported by a type of barcode.
    = IllegalCharacter Char
    | IllegalCode Int
    | GerealError String

instance Error BarcodeError
    where
        noMsg = GerealError "Unknown error"
        strMsg = GerealError

(<?>) :: (MonadError BarcodeError m) => Maybe a -> BarcodeError -> m a
(<?>) Nothing err = throwError err
(<?>) (Just a) _ = return a
