module SumOfMultiples (sumOfMultiples) where

divisibleBy :: Integer -> Integer -> Bool
divisibleBy _ 0 = False
divisibleBy n d = n `mod` d == 0

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum [n | n <- [1..limit - 1], any (n `divisibleBy`) factors]
