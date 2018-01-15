module Day1
    where

d1aPrint::String->IO ()
d1aPrint file = print $ d1a file
        
d1bPrint::String->IO ()
d1bPrint file = print $ d1b file

d1a::String->Integer
d1a file = sumInput $ readInt file

d1b::String->Integer
d1b file = sumInput' $ readInt file

digits :: Integral x=>x->[x]
digits 0 = []
digits x = digits (x `div` 10) ++ [x`mod`10]

readInt :: String -> Integer
readInt = read

prepareList x = digits x ++ [head (digits x)]

sumInput x = checkByNext $ prepareList x

checkByNext [] = 0
checkByNext (x:[]) = 0
checkByNext (x:y:xs) = if x==y then x +checkByNext (y:xs) else checkByNext (y:xs)



sumInput' x =    let       list = prepareList x
                           len = quot (length list) 2 
                           xs = take len list
                           ys = drop len list
                           f x y = if x==y then x else 0 
                    in (2*) $ sum $ zipWith f xs ys 




