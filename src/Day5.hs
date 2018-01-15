module Day5 where
import Data.List

    
d5aPrint::String->IO ()
d5aPrint file = print $ d5a file
d5bPrint::String->IO ()
d5bPrint file = print $ d5b file
    
d5a:: String -> Int
d5a file = countSteps (map readInt (lines file)) 0
d5b:: String -> Int
d5b file = countSteps' (map readInt (lines file)) 0  

readInt :: String -> Int
readInt = read

getNewPosition :: [Int]->Int->Int
getNewPosition xs p = p + (xs !! p)

getNewList :: [Int]->(Int->Int)->Int->[Int]
getNewList xs f p = beg ++ [f (head end)] ++  tail end
    where   beg = take p xs 
            end = drop p xs

countSteps :: [Int]->Int->Int
countSteps xs p
        | p >= (length xs) || p<0 = 0
        | otherwise = 1 + ( countSteps (getNewList xs (\x->x+1) p) (getNewPosition xs p) )

countSteps' :: [Int]->Int->Int
countSteps' xs p
        | p >= (length xs) || p<0 = 0
        | (xs !! p) >= 3 =  1 + ( countSteps' (getNewList xs (\x->x-1) p) (getNewPosition xs p) )
        | otherwise = 1 + ( countSteps' (getNewList xs (\x->x+1) p) (getNewPosition xs p) )