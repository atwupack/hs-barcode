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

import qualified Barcode.Linear.Code11 as B
import Barcode.Linear

printEnc :: Maybe [Bar] -> IO()
printEnc Nothing = print ""
printEnc (Just a) = print a

main = printEnc $ B.encode "123-45"



