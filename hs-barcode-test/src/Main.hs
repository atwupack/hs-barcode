{-# LANGUAGE CPP, TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
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

module Main (
    main
) where

import Barcode.Linear
import Control.Monad.Error


main = do
    runErrorT $ save (Code11 CK) "123-4567890" (Dimensions 10 2 50) "/var/tmp/code11.png"
    runErrorT $ save (Code39 True) "123-456ABCDE" (Dimensions 10 2 50) "/var/tmp/code39.png"
    runErrorT $ save (Code39Extended False) "This's a test!" (Dimensions 10 2 50) "/var/tmp/code39ascii.png"
    runErrorT $ save Code93 "123-4567890" (Dimensions 10 2 50) "/var/tmp/code93.png"
    runErrorT $ save Code93Extended "This's a test!" (Dimensions 10 2 50) "/var/tmp/code93ascii.png"



