module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map, fromList)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

getCount :: Nucleotide -> String -> Int
getCount _ "" = 0
getCount A xs = length $ filter (=='A') xs
getCount C xs = length $ filter (=='C') xs
getCount G xs = length $ filter (=='G') xs
getCount T xs = length $ filter (=='T') xs

isNotValid :: String -> Bool
isNotValid = any (`elem` [x | x <- ['A'..'Z'], x /= 'A', x /= 'C', x /= 'G', x /= 'T'])

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs 
    | isNotValid xs = Left "DNA string not valid"
    | otherwise = Right $ fromList [(A, getCount A xs), (C, getCount C xs), (G, getCount G xs), (T, getCount T xs)]
