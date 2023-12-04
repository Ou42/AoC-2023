module Main where

{-
    Day 03

      Part A
      - given a 2D grid of part numbers and symbols `.` is a blank
      - any number adjacent to a symbol is a part number
      - add up all the part numbers
-}


hr :: IO ()
hr = putStrLn $ replicate 42 '-' ++ ['\n']

partA :: String -> IO ()
partA filePath = do
  fileInput <- readFile filePath
  putStrLn "Day 03 - Part A"

  hr


main :: IO ()
main = do
  let filePath = "input-03.test"
  -- let filePath = "input-02.txt"

  hr

  partA filePath
