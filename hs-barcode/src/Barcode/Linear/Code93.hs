-----------------------------------------------------------------------------
--
-- Module      :  Barcode.Linear.Code93
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

module Barcode.Linear.Code93 (
    Code93(..)
) where

import Barcode.Linear.Common
import Barcode.Linear.Util

data Code93 = Code93 | Code93Extended

instance Encoder Code93 where
    encode Code93Extended s = encode Code93 s
    encode Code93 s = do
        list <- convData encTable s
        let
            listc = list ++ [checkr list 20 47]
            listck = listc ++ [checkr listc 15 47]
        codes <- convIndex encTable listck
        let
            ccodes = concat codes
        return $ startStop ++ concat codes ++ startStop ++ [Black 1]

encTable :: [(Char,[Int])]
encTable = [    ('0',[1,3,1,1,1,2]), ('1',[1,1,1,2,1,3]),
                ('2',[1,1,1,3,1,2]), ('3',[1,1,1,4,1,1]),
                ('4',[1,2,1,1,1,3]), ('5',[1,2,1,2,1,2]),
                ('6',[1,2,1,3,1,1]), ('7',[1,1,1,1,1,4]),
                ('8',[1,3,1,2,1,1]), ('9',[1,4,1,1,1,1]),
                ('A',[2,1,1,1,1,3]), ('B',[2,1,1,2,1,2]),
                ('C',[2,1,1,3,1,1]), ('D',[2,2,1,1,1,2]),
                ('E',[2,2,1,2,1,1]), ('F',[2,3,1,1,1,1]),
                ('G',[1,1,2,1,1,3]), ('H',[1,1,2,2,1,2]),
                ('I',[1,1,2,3,1,1]), ('J',[1,2,2,1,1,2]),
                ('K',[1,3,2,1,1,1]), ('L',[1,1,1,1,2,3]),
                ('M',[1,1,1,2,2,2]), ('N',[1,1,1,3,2,1]),
                ('O',[1,2,1,1,2,2]), ('P',[1,3,1,1,2,1]),
                ('Q',[2,1,2,1,1,2]), ('R',[2,1,2,2,1,1]),
                ('S',[2,1,1,1,2,2]), ('T',[2,1,1,2,2,1]),
                ('U',[2,2,1,1,2,1]), ('V',[2,2,2,1,1,1]),
                ('W',[1,1,2,1,2,2]), ('X',[1,1,2,2,2,1]),
                ('Y',[1,2,2,1,2,1]), ('Z',[1,2,3,1,1,1]),
                ('-',[1,2,1,1,3,1]), ('.',[3,1,1,1,1,2]),
                (' ',[3,1,1,2,1,1]), ('$',[3,2,1,1,1,1]),
                ('/',[1,1,2,1,3,1]), ('+',[1,1,3,1,2,1]),
                ('%',[2,1,1,1,3,1])]

-- | Encoding of the start/stop symbol
startStop :: [Bar]
startStop = convertEnc [1,1,1,1,4,1]
