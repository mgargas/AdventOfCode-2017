module Day2 where

import System.Environment


d2aPrint::String->IO ()
d2aPrint file = print $ d2a file
d2bPrint::String->IO ()
d2bPrint file = print $ d2b file

d2a:: String -> Int
d2a file = checkSum $ map (readInt' . words)  (lines file)
d2b:: String -> Int
d2b file = checkSum2 $ map (readInt' . words)  (lines file)
    

checkSum :: [[Int]]->Int
checkSum xs = sum $ map (\x->(maximum x) - (minimum x)) xs

checkSum2 :: [[Int]]->Int
checkSum2 xs = sum $ concat $ map findDivisor  xs


readInt :: String -> Int
readInt = read

readInt' :: [String]->[Int]
readInt' = map readInt

findDivisor xs = [x`div`y|x<-xs,y<-xs,x/=y,x`mod`y==0] 




