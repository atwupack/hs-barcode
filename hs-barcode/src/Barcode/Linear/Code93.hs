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
import qualified Barcode.Linear.Code39 as C39
import qualified Data.Map as M

-- | Code 93 barcode.
data Code93
    -- | Standard Code 93 barcode supporting 0-9, A-Z and some special characters.
    = Code93
    -- | Extended Code 93 barcode supporting full ASCII charset.
    | Code93Extended

doEncode :: [(Char,[Int])] -> String -> Maybe [Bar]
doEncode table s = do
    list <- convData table s
    let
        listc = list ++ [checkr list 20 47]
        listck = listc ++ [checkr listc 15 47]
    codes <- convIndex table listck
    let
        ccodes = concat codes
    return $ startStop ++ concat codes ++ startStop ++ [Black 1]

instance Encoder Code93 where
    encode Code93Extended s = do
        pureCode93 <- mapM (`M.lookup` asciiMap) s
        doEncode extTable (concat pureCode93)
    encode Code93 s = doEncode encTable s

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

extTable = encTable ++ [    ('§', [1,2,1,2,2,1]), ('‰', [3,1,2,1,1,1]),
                            ('÷', [3,1,1,1,2,1]), ('‡', [1,2,2,2,1,1])]

asciiTable :: [(Char, String)]
asciiTable = map (\x->(fst x, map toExtSymbol (snd x))) C39.asciiTable
    where
        toExtSymbol '%' = '‰'
        toExtSymbol '$' = '§'
        toExtSymbol '+' = '‡'
        toExtSymbol '/' = '÷'
        toExtSymbol c = c

asciiMap :: M.Map Char String
asciiMap = M.fromList asciiTable

-- | Encoding of the start/stop symbol
startStop :: [Bar]
startStop = convertEnc [1,1,1,1,4,1]
