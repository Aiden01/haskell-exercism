
import qualified Data.Set as Set

digits :: Integral x => x -> [x]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

notEqualLength :: Int -> Int -> Bool
notEqualLength n1 n2 = length (digits n1) /= length (digits n2)

notSameDigits :: Int -> Int -> Bool
notSameDigits n1 n2 = Set.size (Set.difference (Set.fromList (digits n2)) (Set.fromList (digits n1))) /= 0

findInteger :: Int -> Int
findInteger 0 = findInteger (1) 
findInteger n
    | notEqualLength n (n * 2) = findInteger (n + 1)
    | notEqualLength n (n * 3) = findInteger (n + 1)
    | notEqualLength n (n * 4) = findInteger (n + 1)
    | notEqualLength n (n * 5) = findInteger (n + 1)
    | notEqualLength n (n * 6) = findInteger (n + 1)
    | notSameDigits n (n * 2) = findInteger (n + 1)
    | notSameDigits n (n * 3) = findInteger (n + 1)
    | notSameDigits n (n * 4) = findInteger (n + 1)
    | notSameDigits n (n * 5) = findInteger (n + 1)
    | notSameDigits n (n * 6) = findInteger (n + 1)
    | otherwise = n