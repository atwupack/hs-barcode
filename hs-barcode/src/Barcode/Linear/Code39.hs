-----------------------------------------------------------------------------
--
-- Module      :  Barcode.Linear.Code39
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

module Barcode.Linear.Code39 (
    encode, Code39(..)
) where

import Barcode.Linear
import Barcode.Linear.Util
import qualified Data.Map as M

data Code39 = Code39 Bool | Code39Ascii Bool

instance Encoder Code39 where
    encode (Code39Ascii check) s = do
        pureCode39 <- mapM (\x->M.lookup x extMap) s
        encode (Code39 check) (concat pureCode39)
    encode (Code39 check) s = do
        list <- convData encTable s
        let
            digits = if check then list ++ [sum list `mod` 43] else list
        codes <- mapM (lookupCode encTable) digits
        let
            ccodes = concatMap (++[White 1]) codes
        return $ startStop ++ [White 1] ++ ccodes ++ [White 1] ++ startStop

encTable :: [(Char, Int, String)]
encTable = [    ('0',0, "bwbWBwBwb"), ('1',1, "BwbWbwbwB"),
                ('2',2, "bwBWbwbwB"), ('3',3, "BwBWbwbwb"),
                ('4',4, "bwbWBwbwB"), ('5',5, "BwbWBwbwb"),
                ('6',6, "bwBWBwbwb"), ('7',7, "bwbWbwBwB"),
                ('8',8, "BwbWbwBwb"), ('9',9, "bwBWbwBwb"),
                ('A',10, "BwbwbWbwB"), ('B',11, "bwBwbWbwB"),
                ('C',12, "BwBwbWbwb"), ('D',13, "bwbwBWbwB"),
                ('E',14, "BwbwBWbwb"), ('F',15, "bwBwBWbwb"),
                ('G',16, "bwbwbWBwB"), ('H',17, "BwbwbWBwb"),
                ('I',18, "bwBwbWBwb"), ('J',19, "bwbwBWBwb"),
                ('K',20, "BwbwbwbWB"), ('L',21, "bwBwbwbWB"),
                ('M',22, "BwBwbwbWb"), ('N',23, "bwbwBwbWB"),
                ('O',24, "BwbwBwbWb"), ('P',25, "bwBwBwbWb"),
                ('Q',26, "bwbwbwBWB"), ('R',27, "BwbwbwBWb"),
                ('S',28, "bwBwbwBWb"), ('T',29, "bwbwBwBWb"),
                ('U',30, "BWbwbwbwB"), ('V',31, "bWBwbwbwB"),
                ('W',32, "BWBwbwbwb"), ('X',33, "bWbwBwbwB"),
                ('Y',34, "BWbwBwbwb"), ('Z',35, "bWBwBwbwb"),
                ('-',36, "bWbwbwBwB"), ('.',37, "BWbwbwBwb"),
                (' ',38, "bWBwbwBwb"), ('$',39, "bWbWbWbwb"),
                ('/',40, "bWbWbwbWb"), ('+',41, "bWbwbWbWb"),
                ('%',42, "bwbWbWbWb")]

-- | Encoding of the start/stop symbol
startStop :: [Bar]
startStop = convertEnc "bWbwBwBwb"

extTable :: [(Char, String)]
extTable = [    ('\NUL', "%U"), (' '," "), ('@', "%V"), ('`', "%W"),
                ('\SOH', "$A"), ('!', "/A"), ('A', "A"), ('a', "+A"),
                ('\STX', "$B"), ('"', "/B"), ('B', "B"), ('b', "+B"),
                ('\ETX', "$C"), ('#', "/C"), ('C', "C"), ('c', "+C"),
                ('\EOT', "$D"), ('$', "/D"), ('D', "D"), ('d', "+D"),
                ('\ENQ', "$E"), ('%', "/E"), ('E', "E"), ('e', "+E"),
                ('\ACK', "$F"), ('&', "/F"), ('F', "F"), ('f', "+F"),
                ('\BEL', "$G"), ('\'', "/G"), ('G', "G"), ('g', "+G"),
                ('\BS',  "$H"), ('(', "/H"), ('H', "H"),  ('h', "+H"),
                ('\HT', "$I"), (')', "/I"), ('I', "I"), ('i', "+I"),
                ('\LF', "$J"), ('*',"/J"), ('J', "J"), ('j', "+J"),
                ('\VT', "$K"), ('+',"/K"), ('K', "K"), ('k', "+K"),
                ('\FF', "$L"), (',',"/L"), ('L', "L"), ('l', "+L"),
                ('\CR', "$M"), ('-',"-"), ('M', "M"), ('m', "+M"),
                ('\SO', "$N"), ('.',"."), ('N', "N"), ('n', "+N"),
                ('\SI', "$O"), ('/',"/O"), ('O', "O"), ('o', "+O"),
                ('\DLE', "$P"), ('0',"0"), ('P', "P"), ('p', "+P"),
                ('\DC1', "$Q"), ('1',"1"), ('Q', "Q"), ('q', "+Q"),
                ('\DC2', "$R"), ('2',"2"), ('R', "R"), ('r', "+R"),
                ('\DC3', "$S"), ('3',"3"), ('S', "S"), ('s', "+S"),
                ('\DC4', "$T"), ('4',"4"), ('T', "T"), ('t', "+T"),
                ('\NAK', "$U"), ('5',"5"), ('U', "U"), ('u', "+U"),
                ('\SYN', "$V"), ('6',"6"), ('V', "V"), ('v', "+V"),
                ('\ETB', "$W"), ('7',"7"), ('W', "W"), ('w', "+W"),
                ('\CAN', "$X"), ('8',"8"), ('X', "X"), ('x', "+X"),
                ('\EM', "$Y"), ('9',"9"), ('Y', "Y"), ('y', "+Y"),
                ('\SUB', "$Z"), (':',"/Z"), ('Z', "Z"), ('z', "+Z"),
                ('\ESC', "%A"), (';',"%F"), ('[', "%K"), ('{', "%P"),
                ('\FS', "%B"), ('<',"%G"), ('\\', "%L"), ('|', "%Q"),
                ('\GS', "%C"), ('=',"%H"), (']', "%M"), ('}', "%R"),
                ('\RS', "%D"), ('>',"%I"), ('^', "%N"), ('~', "%S"),
                ('\US', "%E"), ('?',"%J"), ('_', "%O"), ('\DEL', "%T")]

extMap :: M.Map Char String
extMap = M.fromList $ extTable

