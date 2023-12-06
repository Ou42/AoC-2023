module Main where

import Data.Char ( isDigit )
import qualified Data.Vector as V

{-
    Day 03

      Part A
      - given a 2D grid of part numbers and symbols `.` is a blank
      - any number adjacent to a symbol is a part number
      - add up all the part numbers
-}


hr :: IO ()
hr = putStrLn $ replicate 42 '-' ++ ['\n']

parseA :: String -> [V.Vector Char]
parseA fileInput = map V.fromList $ lines fileInput

partA :: String -> IO ()
partA filePath = do
  fileInput <- readFile filePath
  putStrLn "Day 03 - Part A"

  hr

  -- let symbols = filter (\c -> (c /= '.') && not (isDigit c)) fileInput
  -- let symbols = filter isSymbol fileInput
  -- let symbols = map (filter isPunctuation) $ lines fileInput

  -- let symbols = map (filter (\c -> (c /= '.') && not (isDigit c)))
  --                   $ lines fileInput
  let firstFew ss = unlines $ take 10 [show (i, x) | (x, i) <- zip ss [0..]]

      -- filterSymbols = filter (/= '.') . filter (not . isDigit)
      filterSymbols = filter (\c -> (c /= '.') && not (isDigit c))
      symbols = map filterSymbols $ lines fileInput

      -- filterSymbols' = (<*>) [(/= '.'), not . isDigit]
      filterSymbols' = ([(/= '.'), not . isDigit] <*>)
      symbols' = map filterSymbols $ lines fileInput

      filterSymbols'' = filter (not . flip elem ".0..9")
      symbols'' = map filterSymbols $ lines fileInput

  putStrLn "Symbol List:"
  putStrLn $ firstFew symbols
  putStrLn $ "Symbol count = " <> (show . length . concat) symbols

  hr

  putStrLn "Symbol' List:"
  putStrLn $ firstFew symbols'
  putStrLn $ "Symbol' count = " <> (show . length . concat) symbols'

  hr

  putStrLn "Symbol'' List:"
  putStrLn $ firstFew symbols''
  putStrLn $ "Symbol'' count = " <> (show . length . concat) symbols''

  hr

  putStrLn $ "3 Symbol's count are equal = " <> show (all (symbols ==) [symbols', symbols''])

  -- I'm thinking of:
  --   1. reading/parsing the input data

  hr
  putStr $ unlines $ take 10 $ map show $ parseA fileInput

  --   2. scaning for a number
  --   3. then searching all locations around the number
  --   4. if a non `.` symbol is found, it's a "part number"
  --   5. goto 2, until the whole 2D grid is processed
  --   6. sum the list of "part numbers"

  -- As opposed to:
  --   - scanning for symbols and then trying to extract adjacent numbers

  -- Potentially useful tools:
  --   1. Data.Char.isPunctuation ?!
  --   2. ghci> Data.Maybe.catMaybes [Just 6, Just 7, Nothing]
  --      [6,7]
  --   3. zip line [0..] to get indices
  --   4. use Array's or a data structure that can extract a sub-sequence by indices

main :: IO ()
main = do
  let filePath = "input-03.test"
  -- let filePath = "input-03.txt"

  hr

  partA filePath
