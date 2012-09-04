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

import Barcode.Linear.Code11
import Barcode.Linear.Code39
import Barcode.Linear
import Barcode.Linear.Output

saveEnc :: Maybe [Bar] -> FilePath -> IO()
saveEnc Nothing _ = print "Error"
saveEnc (Just a) path = saveBarcode (Dimensions 10 2 50)  a path


printEnc :: Maybe [Bar] -> IO()
printEnc Nothing = print ""
printEnc (Just a) = print a

main = do
    saveEnc (encode (Code11 CK) "123-456") "/var/tmp/code11.png"
    saveEnc (encode (Code39 True) "123-456") "/var/tmp/code39.png"
    saveEnc (encode (Code39Ascii True) "This's a test!") "/var/tmp/code39ascii.png"



