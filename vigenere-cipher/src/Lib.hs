module Lib
    ( encrypt
    ) where

import Data.Maybe (fromJust)
import Data.List (elemIndex, intercalate)
import Data.Char (toUpper, isLetter, isUpper)

data Size = Upper | Lower

alphabet :: Size -> String
alphabet Upper = ['A'..'Z']
alphabet Lower = ['a'..'z']

transform :: Char -> Char -> Char
transform keyC plainC 
        | not $ isLetter plainC = plainC
        | otherwise = do
            let charSize = if isUpper plainC then Upper else Lower
            getNewChar charSize (letterPos plainC) (letterPos keyC)
            

setKeyLength :: Int -> String -> String
setKeyLength n xs
    | length xs > n =  take n xs
    | otherwise = take n (concat $ replicate (n - length xs) xs)


getNewChar :: Size -> Int -> Int -> Char
getNewChar Upper plainPos keyPos = alphabet Upper !! ((plainPos + keyPos) `mod` 26)
getNewChar Lower plainPos keyPos = alphabet Lower !! ((plainPos + keyPos) `mod` 26)



letterPos :: Char -> Int
letterPos c = fromJust $ toUpper c `elemIndex` alphabet Upper


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
encrypt plain key 
    | length plain == length key = zipWith transform key plain
    | otherwise = do
        let fixedKey = setKeyLength (length plain) (filter isLetter key)
        zipWith transform fixedKey plain

