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
    convertEnc, convData, convIndex, checkr, forceLookup
) where

import qualified Data.Map as M
import Barcode.Linear.Common
import Barcode.Error
import Control.Monad
import Control.Monad.Error

forceLookup :: (MonadError BarcodeError m, Ord a) => a -> M.Map a b -> BarcodeError -> m b
forceLookup key map err = M.lookup key map <?> err

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
lookupChar :: (MonadError BarcodeError m) => [(Char,[Int])] -> Char -> m Int
lookupChar table c = forceLookup c (charMap table) (IllegalCharacter c)

-- | Map to lookup barcode for code
codeMap :: [(Char,[Int])] -> M.Map Int [Bar]
codeMap table = M.fromList $ fmap (\(c,i,s)->(i,convertEnc s)) (indexTable table)

-- | Perform lookup for encoding.
lookupCode :: (MonadError BarcodeError m) => [(Char,[Int])] -> Int -> m [Bar]
lookupCode table c = forceLookup c (codeMap table) (IllegalCode c)

-- | Convert a symbol in internal representation to Bar list
convertEnc :: [Int] -> [Bar]
convertEnc = foldl addBar []

addBar :: [Bar] -> Int -> [Bar]
addBar bar width
    | even (length bar) = bar ++ [Black width]
    | otherwise = bar ++ [White width]

-- | Convert data to index list.
convData :: (MonadError BarcodeError m) => [(Char,[Int])] -> String -> m [Int]
convData table = mapM (lookupChar table)

-- | Convert index list to encodings
convIndex :: (MonadError BarcodeError m) => [(Char,[Int])] -> [Int] -> m [[Bar]]
convIndex table = mapM (lookupCode table)



