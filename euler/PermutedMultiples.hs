
import qualified Data.Set as Set

digits :: Integral x => x -> [x]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

notEqualLength :: Integer -> Integer -> Bool
notEqualLength n1 n2 = length (digits n1) /= length (digits n2)

notSameDigits :: Integer -> Integer -> Bool
notSameDigits n1 n2 = Set.size (Set.difference (Set.fromList (digits n2)) (Set.fromList (digits n1))) /= 0

checkForMultiples :: (Integer -> Integer -> Bool) -> Integer -> Integer -> Bool
checkForMultiples op n k = or [ n `op` (n * i) | i <- [2..k] ]

findInteger :: Integer -> Integer
findInteger 0 = findInteger 1
findInteger n
    | cond      = findInteger (n + 1)
    | otherwise = n
    where cond = checkForMultiples notEqualLength n 6
              || checkForMultiples notSameDigits n 6 