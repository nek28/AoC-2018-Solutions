module Main where

import Data.List

--transforms a string into a "histogram" of how many times each letter is repeated
--in alphabetic order
transf :: String -> [Int]
transf str = fmap length (group . sort $ str)

--checks if any letter is repeated a certain number, which is given as a predicate
--the function itself is more general, accepting any (Int->Bool) predicate
--but is used for that purpose
checkfor :: (Int -> Bool) -> [Int] -> Bool
checkfor p = or . (fmap p)

--In the puzzle, we are asked to compute a simple checksum on the list of input strings
--(2/data), namely the product of the number of strings that have a letter repeating 
--exactly two times and those that have one repeating three times
checksum :: [String] -> Int
checksum strs = twos * threes
    where twos = length $ stringsWithNs 2
          threes = length $ stringsWithNs 3
          stringsWithNs n = filter (checkfor (== n)) $ fmap transf strs

main :: IO ()
main = do
    inp <- readFile "2/data"
    let ids = words inp
    putStrLn $ "Checksum over input is " ++ show (checksum ids)