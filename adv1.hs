module Main where

import qualified Data.IntSet as Set

--gets the input data string (lines of the form "[+-]NUM") and turns it into [Int]
asDigits :: String -> [Int]
asDigits = (fmap (read :: String -> Int)) . words . filterPlus
    where filterPlus = filter (/= '+')

--solves the "first repeating frequency" problem on input
--it computes the partial sums of the elements of (cycle ls)
--and returns the first element that repeat in the Set of partial sums
fRInf :: [Int] -> Int
fRInf ls = fRInf' (cycle ls) 0 (Set.fromList [0])
    where fRInf' (x:xs) acc oldFreqs = let newFreq = acc + x
                                       in if newFreq `Set.member` oldFreqs
                                          then newFreq
                                          else fRInf' xs newFreq (Set.insert newFreq oldFreqs)

main :: IO()
main = do
    inp <- readFile "data/1"
    let wrangled = asDigits inp
    putStrLn $ "The final frequency is " ++ show (sum wrangled)
    putStrLn $ "The first repeating frequency is " ++ show (fRInf wrangled)