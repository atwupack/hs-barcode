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
-- | Implementation if the Code 11 / USD-8 Barcode.
-- This barcode allows the encoding of 0-9 and the dash symbol (-).
-- It also supports the use of up to two optional checksums based on wheighted
-- sums modulo 11.
--
-----------------------------------------------------------------------------

module Barcode.Linear.Code11 (
    encode, CheckDigit(..), Code11(..)
) where

import qualified Data.Map as M
import Data.Char
import Control.Monad
import Barcode.Linear
import Barcode.Linear.Util

data Code11 = Code11 CheckDigit

instance Encoder Code11 where
    encode (Code11 c) s = do
        list <- convData encTable s
        let
            digits = checkData list c
        codes <- mapM (lookupCode encTable) digits
        let
            ccodes = concatMap (++[White 1]) codes
        return $ startStop ++ [White 1] ++ ccodes ++ [White 1] ++ startStop

data CheckDigit
    -- | Do not create any check digit.
    = None
    -- | Create the C check digit.
    | C
    -- | Create the C and the K check digits.
    | CK
    -- | Always create the C digit. The K digit is only created for
    -- messages greater the given length.
    | CKLimit Int

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


-- | Encoding of the start/stop symbol
startStop :: [Bar]
startStop = convertEnc "bwBWb"

-- | Calculate and add check digits
checkData :: [Int] -> CheckDigit -> [Int]
checkData list None = list
checkData list C = list ++ [checkr list 10 11]
checkData list CK = checkData (list ++ [checkr list 9 11]) C
checkData list (CKLimit a)
    | a>length list = checkData list C
    | otherwise = checkData list CK

-- | Calculate the checksum
checkr :: [Int] -> Int -> Int -> Int
checkr values maxCount modulo =
    (snd (foldr (checks maxCount) (1,0) values)) `mod` modulo

checks :: Int -> Int -> (Int,Int) -> (Int,Int)
checks maxCount value (weight,cs) = (newWeight, cs + weight*value)
    where
        newWeight = if weight==maxCount then 1 else weight+1

