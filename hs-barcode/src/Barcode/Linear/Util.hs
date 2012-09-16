-----------------------------------------------------------------------------
--
-- Module      :  Barcode.Linear.Util
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

module Barcode.Linear.Util (
    convertEnc, convData, convIndex, checkr
) where

import qualified Data.Map as M
import Barcode.Linear.Common
import Control.Monad

-- | Calculate the checksum
checkr :: [Int] -> Int -> Int -> Int
checkr values maxCount modulo =
    snd (foldr (checks maxCount) (1,0) values) `mod` modulo

checks :: Int -> Int -> (Int,Int) -> (Int,Int)
checks maxCount value (weight,cs) = (newWeight, cs + weight*value)
    where
        newWeight = if weight==maxCount then 1 else weight+1

-- | Add index entry into a lookup table
indexTable :: [(Char,[Int])] -> [(Char,Int,[Int])]
indexTable = foldl app []
    where
        app x (c,s) = x ++ [(c,length x,s)]

-- | Create lookup Map from table
charMap :: [(Char,[Int])] -> M.Map Char Int
charMap table = M.fromList $ fmap (\(c,i,s)->(c,i)) (indexTable table)

-- | Perform a lookup for a Char
lookupChar :: [(Char,[Int])] -> Char -> Maybe Int
lookupChar table c = M.lookup c (charMap table)

-- | Map to lookup barcode for code
codeMap :: [(Char,[Int])] -> M.Map Int [Bar]
codeMap table = M.fromList $ fmap (\(c,i,s)->(i,convertEnc s)) (indexTable table)

-- | Perform lookup for encoding.
lookupCode :: [(Char,[Int])] -> Int -> Maybe [Bar]
lookupCode table c = M.lookup c (codeMap table)

-- | Convert a symbol in internal representation to Bar list
convertEnc :: [Int] -> [Bar]
convertEnc = foldl addBar []

addBar :: [Bar] -> Int -> [Bar]
addBar bar width
    | even (length bar) = bar ++ [Black width]
    | otherwise = bar ++ [White width]

-- | Convert data to index list.
convData :: [(Char,[Int])] -> String -> Maybe [Int]
convData table = mapM (lookupChar table)

-- | Convert index list to encodings
convIndex :: [(Char,[Int])] -> [Int] -> Maybe [[Bar]]
convIndex table = mapM (lookupCode table)



