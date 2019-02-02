module Hamming (distance) where


getDiff :: String -> String -> Int
getDiff [] _ = 0
getDiff _ [] = 0
getDiff (x:xs) (y:ys) 
    | x == y = 0 + getDiff xs ys
    | otherwise = 1 + getDiff xs ys

distance :: String -> String -> Maybe Int
distance xs ys 
    | length xs /= length ys = Nothing
    | otherwise = Just $ getDiff xs ys
