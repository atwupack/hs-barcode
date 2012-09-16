-----------------------------------------------------------------------------
--
-- Module      :  Barcode.Linear.Output
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

module Barcode.Linear.Output (
    Dimensions(..), saveBarcode
) where

import Barcode.Linear.Common
import Codec.Picture.Types
import Codec.Picture

data Dimensions = Dimensions { border :: Int, barWidth :: Int, barHeight :: Int }

convCode :: [Bar] -> [Bool]
convCode = concatMap convBar
    where
        convBar (Black a) = replicate a True
        convBar (White a) = replicate a False

calcPixel :: Dimensions -> [Bool] -> Int -> Int -> Pixel8
calcPixel dim bits x y
    | x<border dim = 255
    | y<border dim = 255
    | x>=imageWidth-border dim = 255
    | y>=imageHeight-border dim = 255
    | bits!!((x-border dim) `div` barWidth dim) = 0
    | otherwise = 255
    where
        imageWidth = (2*border dim) + (barWidth dim * length bits)
        imageHeight = (2*border dim) + barHeight dim

saveBarcode :: Dimensions -> [Bar] -> FilePath -> IO()
saveBarcode dim code path = do
    print imageWidth
    print imageHeight
    savePngImage path (ImageY8 image)
    where
        bits = convCode code
        imageWidth = (2*border dim) + (barWidth dim * length bits)
        imageHeight = (2*border dim) + barHeight dim
        image = generateImage (calcPixel dim bits) imageWidth imageHeight
