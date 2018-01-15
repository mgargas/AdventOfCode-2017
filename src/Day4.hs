module Day4 where

import Data.List 
import Data.Char


d4aPrint::String->IO ()
d4aPrint file = print $ d4a file
d4bPrint::String->IO ()
d4bPrint file = print $ d4b file

d4a::String->Int
d4a file = countFalses $ map (isNextTheSame . sort) $ map words (lines file)
d4b::String->Int
d4b file = countFalses $ map (isNextTheSame . sort) $ map (map sort) $ map words (lines file)

isNextTheSame :: [String] -> Bool
isNextTheSame []       = False
isNextTheSame (x:[])   = False
isNextTheSame (x:y:xs) = (x == y) || isNextTheSame (y:xs)  


countFalses :: [Bool] -> Int
countFalses [] = 0
countFalses (x:xs) = if x then 0 + countFalses xs else 1 + countFalses xs