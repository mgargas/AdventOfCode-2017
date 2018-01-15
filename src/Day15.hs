module Day15 where
import Data.List 
import Data.Char

d15aPrint::String->IO ()
d15aPrint file = print $ d15a file
d15bPrint::String->IO ()
d15bPrint file = print $ d15b file
    
d15a:: String -> Int
d15a file = generatePairsAndCount 0 0 (makeNewxWithoutCondition $ head l) (makeNewyWithoutCondition $ last l)
                where l = map (readInt . last . words) (lines file)
d15b:: String -> Int
d15b file = generatePairsAndCount1 0 0 (makeNewx $ head l) (makeNewy $ last l)
                where l = map (readInt . last . words) (lines file)

generatePairsAndCount :: Int->Int->Int->Int->Int
generatePairsAndCount p c x y
        | p == 40000001 = c
        | otherwise     = generatePairsAndCount (p+1) (c + checkIfBinaryIsOk x y) (makeNewxWithoutCondition x) (makeNewyWithoutCondition y)
                
                
generatePairsAndCount1 :: Int->Int->Int->Int->Int
generatePairsAndCount1 p c x y
        | p == 5000001 = c
        | otherwise     = generatePairsAndCount1 (p+1) (c + checkIfBinaryIsOk x y) (makeNewx x) (makeNewy y)
                
makeNewxWithoutCondition :: Int -> Int
makeNewxWithoutCondition x = (x*16807) `mod` 2147483647  
                
makeNewyWithoutCondition :: Int -> Int
makeNewyWithoutCondition y = (y*48271) `mod` 2147483647
                
makeNewx :: Int -> Int
makeNewx x = if ((x*16807) `mod` 2147483647 ) `mod` 4 == 0 then (x*16807) `mod` 2147483647 else makeNewx ((x*16807) `mod` 2147483647)
                
makeNewy :: Int -> Int
makeNewy y = if ((y*48271) `mod` 2147483647) `mod` 8 == 0 then (y*48271) `mod` 2147483647 else makeNewy ((y*48271) `mod` 2147483647)
                
checkIfBinaryIsOk :: Int -> Int -> Int
checkIfBinaryIsOk x y  = if (lastNDecToBin [] 16 x) == (lastNDecToBin [] 16 y) then 1 else 0
                
readInt :: String -> Int
readInt = read
                
readInt' :: [String]->[Int]
readInt' = map readInt    
                
lastNDecToBin :: [Int] -> Int -> Int -> [Int]
lastNDecToBin acc counter n
        | counter == 0 = acc
        | otherwise    = lastNDecToBin ((n `mod` 2):acc) (counter - 1) (n `div` 2)
                