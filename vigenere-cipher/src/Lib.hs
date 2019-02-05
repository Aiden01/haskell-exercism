module Lib
    ( encrypt
    ) where

import Data.Maybe (fromJust)
import Data.List (elemIndex)
import Data.Char (toLower, isLetter, isUpper, ord)

data Size = Upper | Lower
data Action = Decrypt | Encrypt

alphabet :: Size -> String
alphabet Upper = ['A'..'Z']
alphabet Lower = ['a'..'z']

transform :: Action -> Char -> Char -> Char
transform action keyC plainC 
        | not $ isLetter plainC = plainC
        | otherwise = do
            let charSize = if isUpper plainC then Upper else Lower
            getNewChar action charSize (fromJust $ letterPos plainC) (fromJust $ letterPos keyC)


setKeyLength :: Int -> String -> String
setKeyLength n xs = take n (cycle xs)


getNewChar :: Action -> Size -> Int -> Int -> Char
getNewChar Encrypt size plainPos keyPos = alphabet size !! ((plainPos + keyPos) `mod` 26)
getNewChar Decrypt size plainPos keyPos = alphabet size !! ((plainPos - keyPos) `mod` 26)




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
        zipWith (transform Encrypt) fixedKey plain

-- | Tests
--
-- Examples:
--
-- >>> decrypt "nsydh" "computer"
-- "lemon"
-- >>> decrypt "NsYdH" "computer"
-- "LeMoN"
-- >>> decrypt "omstv2" "hi"
-- "hello2"

decrypt :: String -> String -> String 
decrypt text key = do
    let fixedKey = setKeyLength (length text) (filter isLetter key)
    zipWith (transform Decrypt) fixedKey text

