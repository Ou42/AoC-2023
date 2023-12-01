module Main where

import Data.Char (isDigit)
import Data.List (isPrefixOf)

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

-- partB :: String -> Int
-- partB fileStr = sum $ map go $ lines fileStr
partB fileStr = sum $ map go $ lines fileStr
  where
    numWords = [ "one", "two", "three"
               , "four", "five", "six"
               , "seven", "eight", "nine"
               ]
    go :: String -> Int
    go lineStr = undefined

main :: IO ()
main = do
  -- fileInput <- readFile "input-01-part-a.test"
  -- fileInput <- readFile "input-01-part-b.test"
  fileInput <- readFile "input-01.txt"

  putStrLn "Day 01 - Part A"
  putStrLn fileInput

  hr

  print $ "Part A = " ++ show (partA fileInput)
