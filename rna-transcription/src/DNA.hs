module DNA (toRNA) where

import Data.Maybe (fromJust)
import Data.List (find)


isValid :: Char -> Bool
isValid c = c == 'A' || c == 'G' || c == 'T'  || c == 'C'

transform :: Char -> Char
transform 'A' = 'U'
transform 'T' = 'A'
transform 'C' = 'G'
transform 'G' = 'C'





toRNA :: String -> Either Char String
toRNA xs 
    | all isValid xs = Right $ map transform xs
    | otherwise = Left $ fromJust $ find (not . isValid) xs
