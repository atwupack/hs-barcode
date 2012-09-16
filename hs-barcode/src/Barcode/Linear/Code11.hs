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
    CheckDigit(..), Code11(..)
) where

import qualified Data.Map as M
import Data.Char
import Control.Monad
import Barcode.Linear.Common
import Barcode.Linear.Util

data Code11 = Code11 CheckDigit

instance Encoder Code11 where
    encode (Code11 c) s = do
        list <- convData encTable s
        let
            digits = checkData list c
        codes <- convIndex encTable digits
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
encTable :: [(Char, [Int])]
encTable = [    ('0', [1,1,1,1,2]),
                ('1', [2,1,1,1,2]),
                ('2', [1,2,1,1,2]),
                ('3', [2,2,1,1,1]),
                ('4', [1,1,2,1,2]),
                ('5', [2,1,2,1,1]),
                ('6', [1,2,2,1,1]),
                ('7', [1,1,1,2,2]),
                ('8', [2,1,1,2,1]),
                ('9', [2,1,1,1,1]),
                ('-', [1,1,2,1,1])]


-- | Encoding of the start/stop symbol
startStop :: [Bar]
startStop = convertEnc [1,1,2,2,1]

-- | Calculate and add check digits
checkData :: [Int] -> CheckDigit -> [Int]
checkData list None = list
checkData list C = list ++ [checkr list 10 11]
checkData list CK = checkData (list ++ [checkr list 9 11]) C
checkData list (CKLimit a)
    | a>length list = checkData list C
    | otherwise = checkData list CK


