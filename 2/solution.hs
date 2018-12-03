module Main where

import Data.List

transf :: String -> [Int]
transf str = fmap length (group . sort $ str)

checkfor :: (Int -> Bool) -> [Int] -> Bool
checkfor p = or . (fmap p)

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