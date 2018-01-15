module Day11 where
import Data.List 
import Data.Char
import Data.List.Split
import Data.Char


d11aPrint::String->IO ()
d11aPrint file = print $ d11a file
d11bPrint::String->IO ()
d11bPrint file = print $ d11b file

d11a::String->Float
d11a file =  steps $ sumTables $  unzip $ map  convertChars $ splitOn "," $ trim file
                
d11b::String->Float
d11b file =  maximum $ map steps $ zip' $ editTuple $ unzip $ map convertChars $ splitOn "," $ trim file

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

convertChars :: [Char] -> (Float,Float)
convertChars x 
    |x == "n"  = (1,0)
    |x == "ne" = (0.5,1)
    |x == "se" = (-0.5,1)
    |x == "s"  = (-1,0)
    |x == "sw" = (-0.5,-1)
    |x == "nw" = (0.5,-1)
    |otherwise = (0,0)


sumTables :: ([Float],[Float])->(Float,Float)
sumTables (x,y) = (foldl1 (+) x, foldl1 (+) y)

steps :: (Float,Float) -> Float
steps (x,y) = if ( (abs x)-((abs y)/2)) > 0 then (abs y) + ( (abs x)-((abs y)/2)) else abs y

dir = loop 0 []
    where
    loop prev acc [] = acc
    loop prev acc (x:xs) = loop (x+prev) ((x+prev):acc) xs

makeDistances = reverse . dir

editTuple :: ([Float],[Float]) -> ([Float],[Float])
editTuple (x,y) = (makeDistances x, makeDistances y)

zip' (x,y) = zip x y

--makeListOfCoordinates :: ([Float],[Float]) -> [(Float,Float)]
--makeListOfCoordinates = map steps $ makeTuples 