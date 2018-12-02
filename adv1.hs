module Main where

import Data.List
import qualified Data.IntSet as S

asDigits :: String -> [Int]
asDigits = (fmap (read :: String -> Int)) . words . filterPlus
    where filterPlus = filter (/= '+')

firstRepeat :: ([Int],[Int]) -> Int -> Int
firstRepeat (st,(x:xs)) acc = if not (newV `elem` st)
                              then firstRepeat (newV:st,xs) newV
                              else newV
                                where newV = acc + x

firstRepeat' :: [Int] -> Int
firstRepeat' ls = firstRepeat' ls 0 (S.fromList [0])
    where firstRepeat' [] acc partial = firstRepeat' ls acc partial
          firstRepeat' (x:xs) acc partial = let newV = x + acc in
                                            if newV `S.member` partial
                                            then newV
                                            else firstRepeat' xs newV (S.insert newV partial)

fpwrap :: [Int] -> Int
fpwrap ls = firstRepeat ([0],cycle ls) 0

wrangle :: String -> Int
wrangle = sumit . asDigits
    where sumit = foldl' (+) 0

main :: IO()
main = do
    str <- readFile "1"
    putStrLn $ show $ firstRepeat' . asDigits $ str