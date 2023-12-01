module Main where

import Data.Char (isDigit)

{-
    Day 01

      Part A
      . Given a list alphanumeric data, extract the first and last digits
        from each line
      . Put them together to form a 2 digit number
      . Sum each line's numbers
-}

hr :: IO ()
hr = putStrLn $ replicate 42 '-' ++ ['\n']

-- partA :: String -> String
partA fileStr = sum $ map go $ lines fileStr
go :: String -> Int
go lineStr =
  let leftDigit = head . snd $ break isDigit lineStr
      rghtDigit = head . snd $ break isDigit $ reverse lineStr
  in read $ leftDigit : [rghtDigit]

main :: IO ()
main = do
  fileInput <- readFile "input-01.test"
  -- fileInput <- readFile "input-01.txt"

  putStrLn "Day 01 - Part A"
  putStrLn fileInput

  hr

  print $ "Part A = " ++ show (partA fileInput)
