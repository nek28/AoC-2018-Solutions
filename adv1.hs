module Main where

import qualified Data.IntSet as S

--gets the input data string (lines of the form "[+-]NUM") and turns it into [Int]
asDigits :: String -> [Int]
asDigits = (fmap (read :: String -> Int)) . words . filterPlus
    where filterPlus = filter (/= '+')

--solves the "first repeating frequency" problem on input
fRInf :: [Int] -> Int
fRInf ls = fRInf' (cycle ls) 0 (S.fromList [0])
    where fRInf' (x:xs) acc oldFreqs = let newFreq = acc + x
                                       in if newFreq `S.member` oldFreqs
                                          then newFreq
                                          else fRInf' xs newFreq (S.insert newFreq oldFreqs)

main :: IO()
main = do
    str <- readFile "data/1"
    let wrangled = asDigits str
    putStrLn $ "The final frequency is " ++ show (sum wrangled)
    putStrLn $ "The first repeating frequency is " ++ show (fRInf wrangled)