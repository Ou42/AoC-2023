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

go2 :: String -> Maybe Int
go2 []    = Nothing
go2 chars =
  let lookUp = map (`isPrefixOf` chars) numWords
  in  if all not lookUp
        then go2 $ tail chars
        else (+1) <$> elemIndex True lookUp


findLast :: Maybe String -> Maybe Int
findLast Nothing    = Nothing
findLast (Just str) = go3 Nothing str
  where
    go3 accu []    = accu
    go3 accu chars@(c:_) =
      let lookUp = map (`isPrefixOf` chars) numWords
      in  if isDigit c
            then go3 (Just (read [c])) $ tail chars
            else if all not lookUp
                    then go3 accu $ tail chars
                    else go3 ((+1) <$> elemIndex True lookUp) $ tail chars


-- using: isPrefixOf & elemIndex
partB :: String -> Int
partB fileStr = sum $ map go $ lines fileStr
  where
    go :: [Char] -> Int
    go lineStr =
      let (upUntilDigit, afterFirstDigit) = break isDigit lineStr
          (firstDigit, rest) = if null afterFirstDigit
                                  then (Nothing, Just upUntilDigit)
                                  else ( Just $ read [head afterFirstDigit]
                                       , Just $ tail afterFirstDigit)
          (d1, d2, d3) = (go2 upUntilDigit, firstDigit, findLast rest)
          tens = if null d1
                    then fromJust d2
                    else fromJust d1
          ones = if null d3
                    then tens
                    else fromJust d3
      in 10 * tens + ones


-- allNums :: String -> Maybe Int
allNums :: [Char] -> [Int]
allNums []    = []
allNums chars@(c:rest) =
  let lookUp = map (`isPrefixOf` chars) numWords
  in  if isDigit c
        then read [c] : allNums rest
        else if all not lookUp
          then allNums rest
          else (+1) (fromJust (elemIndex True lookUp)) : allNums rest

allNumsAsChars []    = [[]]
allNumsAsChars chars@(c:rest) =
  let lookUp = map (`isPrefixOf` chars) numWords
  in  if isDigit c
        then [c] : allNumsAsChars rest
        else if all not lookUp
          then allNumsAsChars rest
          else show ((+1) (fromJust (elemIndex True lookUp))) : allNumsAsChars rest

-- partB2 fileStr = sum $ map go $ lines fileStr
partB2 :: String -> Int
partB2 fileStr = sum
                 $ map ( read
                         . (\ns -> [head ns, last ns])
                         . concat
                         . allNumsAsChars ) $ lines fileStr
-- 281
-- partB2 :: String -> [Int]
-- partB2 fileStr = map ( read
--                        . (\ns -> [head ns, last ns])
--                        . concat
--                        . allNumsAsChars ) $ lines fileStr
-- [29,83,13,24,42,14,76]
-- partB2 fileStr = map ((\ns -> [head ns, last ns])
--                       . concat
--                       . allNumsAsChars) $ lines fileStr
-- ["29","83","13","24","42","14","76"]
-- partB2 fileStr = map (concat . allNumsAsChars) $ lines fileStr
-- ["219","823","123","2134","49872","18234","76"]
-- partB2 fileStr = map allNums $ lines fileStr
-- [[2,1,9],[8,2,3],[1,2,3],[2,1,3,4],[4,9,8,7,2],[1,8,2,3,4],[7,6]]

main :: IO ()
main = do
  fileInputA <- readFile "input-01-part-a.test"
  fileInputB <- readFile "input-01-part-b.test"
  -- fileInputA <- readFile "input-01.txt"
  -- fileInputB <- readFile "input-01.txt"

  putStrLn "Day 01 - Part A"
  putStrLn $ unlines $ take 3 $ lines fileInputA

  hr

  print $ "Part A = " ++ show (partA fileInputA)

  hr

  print $ "Part B = " ++ show (partB fileInputB)
  -- "Part B = 54227" ...
  -- "That's not the right answer; your answer is too low.
  --  If you're stuck, make sure you're using the full input data;
  --  there are also some general tips on the about page, or you
  --  can ask for hints on the subreddit. Please wait one minute
  --  before trying again. [Return to Day 1]"

  hr

  print $ "Part B2 = " ++ show (partB2 fileInputB)
