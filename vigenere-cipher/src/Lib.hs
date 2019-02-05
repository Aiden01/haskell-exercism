module Lib
    ( encrypt
    ) where

import Data.Maybe (fromJust)
import Data.List (elemIndex)
import Data.Char (toLower, isLetter, isUpper, ord)

data Size = Upper | Lower

alphabet :: Size -> String
alphabet Upper = ['A'..'Z']
alphabet Lower = ['a'..'z']

transform :: Char -> Char -> Char
transform keyC plainC 
        | not $ isLetter plainC = plainC
        | otherwise = do
            let charSize = if isUpper plainC then Upper else Lower
            getNewChar charSize (fromJust $ letterPos plainC) (fromJust $ letterPos keyC)


setKeyLength :: Int -> String -> String
setKeyLength n xs = take n (cycle xs)


getNewChar :: Size -> Int -> Int -> Char
getNewChar size plainPos keyPos = alphabet size !! ((plainPos + keyPos) `mod` 26)




letterPos :: Char -> Maybe Int
letterPos c
    | isLetter c = Just (ord (toLower c) - ord 'a')
    | otherwise  = Nothing


-- | Tests
--
-- Examples:
--
-- >>> encrypt "lemon" "computer"
-- "nsydh"
-- >>> encrypt "LeMoN" "computer"
-- "NsYdH"
-- >>> encrypt "hello2" "hi"
-- "omstv2"

encrypt :: String -> String -> String
encrypt plain key = do
        let fixedKey = setKeyLength (length plain) (filter isLetter key)
        zipWith transform fixedKey plain

