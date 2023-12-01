module Main where

import Data.Char (isDigit)
import Data.List (elemIndex, isPrefixOf)
import Data.Maybe (fromJust)

{-
    Day 01

      Part A
      . Given a list alphanumeric data, extract the first and last digits
        from each line
      . Put them together to form a 2 digit number
      . Sum each line's numbers

      Part B
      . ... some of the digits are actually spelled out with letters:
            one, two, three, four, five, six, seven, eight, and nine
        also count as valid "digits".
      . the rest is the same, find the first and last "digits"
      . combine them into a 2 digit number
      . sum each line's numbers
-}

hr :: IO ()
hr = putStrLn $ replicate 42 '-' ++ ['\n']

partA :: String -> Int
partA fileStr = sum $ map go $ lines fileStr
  where
    go :: String -> Int
    go lineStr =
      let leftDigit = head . snd $ break isDigit lineStr
          rghtDigit = head . snd $ break isDigit $ reverse lineStr
      in read $ leftDigit : [rghtDigit]


numWords = [ "one", "two", "three"
           , "four", "five", "six"
           , "seven", "eight", "nine"
           ]


allNumsAsChars :: [Char] -> [[Char]]
allNumsAsChars []    = [[]]
allNumsAsChars chars@(c:rest) =
  let lookUp = map (`isPrefixOf` chars) numWords
  in  if isDigit c
        then [c] : allNumsAsChars rest
        else if all not lookUp
          then allNumsAsChars rest
          else show ((+1) (fromJust (elemIndex True lookUp))) : allNumsAsChars rest

partB :: String -> Int
partB fileStr = sum
                 $ map ( read
                         . (\ns -> [head ns, last ns])
                         . concat
                         . allNumsAsChars
                       )
                       $ lines fileStr

main :: IO ()
main = do
  -- fileInputA <- readFile "input-01-part-a.test"
  -- fileInputB <- readFile "input-01-part-b.test"
  fileInputA <- readFile "input-01.txt"
  fileInputB <- readFile "input-01.txt"

  putStrLn "Day 01 - Part A"
  putStrLn $ unlines $ take 3 $ lines fileInputA

  hr

  print $ "Part A = " ++ show (partA fileInputA)

  hr

  print $ "Part B = " ++ show (partB fileInputB)
