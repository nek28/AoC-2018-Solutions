module Main where

import Data.List
import qualified Data.IntSet as S

asDigits :: String -> [Int]
asDigits = (fmap (read :: String -> Int)) . words . filterPlus
    where filterPlus = filter (/= '+')

firstRepeat' :: [Int] -> Int
firstRepeat' ls = firstRepeat' ls 0 (S.fromList [0])
    where firstRepeat' [] acc partial = firstRepeat' ls acc partial
          firstRepeat' (x:xs) acc partial = let newV = x + acc in
                                            if newV `S.member` partial
                                            then newV
                                            else firstRepeat' xs newV (S.insert newV partial)

wrangle :: String -> Int
wrangle = sumit . asDigits
    where sumit = foldl' (+) 0

main :: IO()
main = do
    str <- readFile "data/1"
    let wrangled = asDigits str
    putStrLn $ "The final frequency is " ++ show (sum wrangled)
    putStrLn $ "The first repeating frequency is " ++ show (firstRepeat' wrangled)