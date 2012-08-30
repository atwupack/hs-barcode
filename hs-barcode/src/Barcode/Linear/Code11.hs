-----------------------------------------------------------------------------
--
-- Module      :  Barcode.Linear.Code11
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

module Barcode.Linear.Code11 (
    encode
) where

import qualified Data.Map as M
import Data.Char
import Control.Monad
import Barcode.Linear

-- | Table encoding the valid characters
-- b : narrow black
-- w : narror white
-- B ; wide black
-- W : wide white
encTable :: [(Char, Int, String)]
encTable = [    ('0',0, "bwbwB"),
                ('1',1, "BwbwB"),
                ('2',2, "bWbwB"),
                ('3',3, "BWbwb"),
                ('4',4, "bwBwB"),
                ('5',5, "BwBwb"),
                ('6',6, "bWBwb"),
                ('7',7, "bwbWB"),
                ('8',8, "BwbWb"),
                ('9',9, "Bwbwb"),
                ('-',10, "bwBwb")]

-- | Map to lookup code for a given character
charMap :: M.Map Char Int
charMap = M.fromList $ fmap (\(c,i,s)->(c,i)) encTable

-- | Map to lookup barcode for code
codeMap :: M.Map Int [Bar]
codeMap = M.fromList $ fmap (\(c,i,s)->(i,convertEnc s)) encTable

-- | Encoding of the start/stop symbol
startStop :: [Bar]
startStop = convertEnc "bwBWb"

-- | Convert a character of the internal representation to the Bar type
convBar :: Char -> Bar
convBar 'b' = Black 1
convBar 'w' = White 1
convBar 'B' = Black 2
convBar 'W' = White 2

-- | Convert a symbol in internal representation to Bar list
convertEnc :: String -> [Bar]
convertEnc = fmap convBar

-- | Encode a character in Code11
encodeChar :: Char -> Maybe ([Bar],Int)
encodeChar c = do
    code <- M.lookup c charMap
    bars <- M.lookup code codeMap
    return (bars, code)

-- | Append a new character encoding to a list
appendChar :: ([Bar], [Int]) -> Char -> Maybe ([Bar], [Int])
appendChar ([],[]) c = do
    ec <- encodeChar c
    return (fst ec, [snd ec])
appendChar (e,i) c = do
    ec <- encodeChar c
    return (e ++ [White 1] ++ fst ec, i ++ [snd ec])

-- | Calculate the checksum
checkr :: [Int] -> Int -> Int -> Int
checkr values maxCount modulo =
    (snd (foldr (checks maxCount) (1,0) values)) `mod` modulo

checks :: Int -> Int -> (Int,Int) -> (Int,Int)
checks maxCount value (weight,cs) = (newWeight, cs + weight*value)
    where
        newWeight = if weight==maxCount then 1 else weight+1

-- | Encode a given String as barcode
encode :: String -> Maybe [Bar]
encode s = do
    ed <- foldM appendChar ([],[]) s
    let csum = checkr (snd ed) 10 11
        ksum = checkr ((snd ed) ++ [csum]) 9 11
    cbar <- M.lookup csum codeMap
    kbar <- M.lookup ksum codeMap
    return $ startStop ++ [White 1] ++ fst ed ++ [White 1] ++ cbar ++ [White 1] ++ kbar ++ [White 1] ++ startStop


