module Isogram (isIsogram) where

import Data.Char (isLetter, toLower)
import Data.List (nub)

getLetters :: String -> String
getLetters = map toLower . filter isLetter

isIsogram :: String -> Bool
isIsogram xs = getLetters xs == nub (getLetters xs)
