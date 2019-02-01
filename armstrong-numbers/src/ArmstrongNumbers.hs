module ArmstrongNumbers (armstrong) where

digits :: Integral x => x -> [x]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

armstrong :: Integral a => a -> Bool
armstrong n = n == sum [x ^ length (digits n) | x <- digits n] 
