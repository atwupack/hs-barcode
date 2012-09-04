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
    lookupChar, convertEnc, convData, lookupCode, concatMapM
) where

import qualified Data.Map as M
import Barcode.Linear
import Control.Monad

-- | Create lookup Map from table
charMap :: [(Char,Int,String)] -> M.Map Char Int
charMap table = M.fromList $ fmap (\(c,i,s)->(c,i)) table

-- | Perform a lookup for a Char
lookupChar :: [(Char,Int,String)] -> Char -> Maybe Int
lookupChar table c = M.lookup c (charMap table)

-- | Map to lookup barcode for code
codeMap :: [(Char,Int,String)] -> M.Map Int [Bar]
codeMap table = M.fromList $ fmap (\(c,i,s)->(i,convertEnc s)) table

-- | Perform lookup for encoding.
lookupCode :: [(Char,Int,String)] -> Int -> Maybe [Bar]
lookupCode table c = M.lookup c (codeMap table)

-- | Convert a symbol in internal representation to Bar list
convertEnc :: String -> [Bar]
convertEnc = fmap convBar

-- | Convert a character of the internal representation to the Bar type
convBar :: Char -> Bar
convBar 'b' = Black 1
convBar 'w' = White 1
convBar 'B' = Black 2
convBar 'W' = White 2

-- | Convert data to index list.
convData :: [(Char,Int,String)] -> String -> Maybe [Int]
convData table s = mapM (lookupChar table) s

-- | The 'concatMapM' function generalizes 'concatMap' to arbitrary monads.
concatMapM        :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs   =  liftM concat (mapM f xs)



