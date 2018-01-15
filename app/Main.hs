module Main where

import System.Environment
import System.IO
import Data.List

import Day1
import Day2
import Day4
import Day5
import Day11
import Day15

dispatch :: [(String,String -> IO ())]  
dispatch =  [ ("Day1a", d1aPrint)  
            , ("Day1b", d1bPrint)
            , ("Day2a", d2aPrint)  
            , ("Day2b", d2bPrint)
            , ("Day4a", d4aPrint)  
            , ("Day4b", d4bPrint)
            , ("Day5a", d5aPrint)  
            , ("Day5b", d5bPrint)
            , ("Day11a", d11aPrint)  
            , ("Day11b", d11bPrint)
            , ("Day15a", d15aPrint)  
            , ("Day15b", d15bPrint)
            ]  

main :: IO () 
main = do 
    (mode:filePath:_) <- getArgs  
    let (Just action) = lookup mode dispatch  
    file <- readFile filePath
    action file 
    

    



