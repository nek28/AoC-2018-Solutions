module Main where

import Data.List

data ListZipper a = Zip ![a] a ![a]
    deriving (Show)

instance Functor ListZipper where
    fmap f (Zip left cur right) = Zip (fmap f left) (f cur) (fmap f right)

toLeft :: ListZipper a -> ListZipper a
toLeft (Zip l c (x:xs)) = Zip (c:l) x xs

fromList :: [a] -> ListZipper a
fromList (x:xs) = Zip [] x xs

--transforms a string into a "histogram" of how many times each letter is repeated
--in alphabetic order
repeats :: String -> [Int]
repeats str = fmap length (group . sort $ str)

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
          stringsWithNs n = filter (checkfor (== n)) $ fmap repeats strs
        
--assumes that the strings are of equal length
hamming :: String -> String -> Int
hamming s t = ham' s t 0
    where ham' [] [] acc = acc
          ham' (x:xs) (y:ys) acc
            | x == y    = ham' xs ys acc
            | otherwise = ham' xs ys (acc + 1)

difference :: String -> String -> String
difference (x:xs) (y:ys) = if x == y
                           then x : difference xs ys
                           else difference xs ys
difference _ _ = []

rightBoxes :: ListZipper String -> (String,String)
rightBoxes zipper@(Zip l cursor r) = if null oneAway
                                     then rightBoxes (toLeft zipper)
                                     else (cursor, head oneAway)
    where oneAway = inls (mapToHam l) ++ inls (mapToHam r)
          inls [] = []
          inls ((n,str):rs) = if n == 1
                              then [str]
                              else inls rs  
          mapToHam = fmap (\s -> (hamming cursor s, s))

zz = Zip [] "koal" ["bear","wear","mish"]

main :: IO ()
main = do
    inp <- readFile "2/data"
    let ids = words inp
    putStrLn $ "Checksum over input is " ++ show (checksum ids)
    let (rightBox,leftBox) = rightBoxes . fromList $ ids
    putStrLn $ "The common substring of the right boxes is " ++ (difference rightBox leftBox)