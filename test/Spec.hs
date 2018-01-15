
import Test.HUnit

import Day15
import Day11
import Day5
import Day4
import Day2
import Day1

main :: IO Counts
main = runTestTT $ tests


test1 = TestCase $ assertEqual "Day1a Test" 3 (d1a "1122")
test2 = TestCase $ assertEqual "Day1a Test" 4 (d1a "1111")
test3 = TestCase $ assertEqual "Day1a Test" 0 (d1a "1234")
test4 = TestCase $ assertEqual "Day1a Test" 9 (d1a "91212129")
tests1a = TestList [test1,test2,test3,test4]

test5 = TestCase $ assertEqual "Day1b Test" 6 (d1b "1212")
test6 = TestCase $ assertEqual "Day1b Test" 0 (d1b "1221")
test7 = TestCase $ assertEqual "Day1b Test" 12 (d1b "123123")
test8 = TestCase $ assertEqual "Day1b Test" 4 (d1b "12131415")
tests1b = TestList [test5,test6,test7,test8]

test2a = TestCase $ assertEqual "Day2a Test" 18 (d2a ("5 1 9 5" ++ "\n" ++ "7 5 3" ++ "\n" ++ "2 4 6 8"))

test2b = TestCase $ assertEqual "Day2b Test" 9 (d2b ("5 9 2 8" ++ "\n" ++ "9 4 7 3" ++ "\n" ++ "3 8 6 5"))

test4a = TestCase $ assertEqual "Day4a Test" 2 (d4a ("aa bb cc dd ee" ++ "\n" ++ "aa bb cc dd aa" ++ "\n" ++ "aa bb cc dd aaa"))

test4b = TestCase $ assertEqual "Day4b Test" 3 (d4b ("abcde fghij" ++ "\n" ++ "abcde xyz ecdab" ++ "\n" ++ "a ab abc abd abf abj" ++"\n" ++ "iiii oiii ooii oooi oooo" ++"\n" ++ "oiii ioii iioi iiio"))

test5a = TestCase $ assertEqual "Day5a Test" 5 (d5a ("0" ++ "\n" ++ "3" ++ "\n" ++ "0" ++ "\n" ++ "1" ++ "\n" ++ "-3"))
test5b = TestCase $ assertEqual "Day5b Test" 10 (d5b ("0" ++ "\n" ++ "3" ++ "\n" ++ "0" ++ "\n" ++ "1" ++ "\n" ++ "-3"))

test9 = TestCase $ assertEqual "Day11a Test" 3 (d11a "ne,ne,ne") 
test10 = TestCase $ assertEqual "Day11a Test" 0 (d11a "ne,ne,sw,sw") 
test11 = TestCase $ assertEqual "Day11a Test" 2 (d11a "ne,ne,s,s" )
test12 = TestCase $ assertEqual "Day11a Test" 3 (d11a "se,sw,se,sw,sw")
tests11a = TestList [test9,test10,test11,test12]

test13 = TestCase $ assertEqual "Day11b Test" 3 (d11b "ne,ne,ne") 
test14 = TestCase $ assertEqual "Day11b Test" 2 (d11b "ne,ne,sw,sw") 
test15 = TestCase $ assertEqual "Day11b Test" 2 (d11b "ne,ne,s,s" )
test16 = TestCase $ assertEqual "Day11b Test" 3 (d11b "se,sw,se,sw,sw")
tests11b = TestList [test13,test14,test15,test16]

test17 = TestCase $ assertEqual "Day15a generatePairsAndCount Test" 588 (generatePairsAndCount 0 0 65 8921)

test18 = TestCase $ assertEqual "Day15b generatePairsAndCount1 Test" 309 (generatePairsAndCount1 0 0 65 8921)

tests = TestList [tests1a,tests1b,test2a,test2b,test4a,test4b,test5b,tests11a,tests11b,test17,test18]