module Main where

import Data.Char ( isDigit )
import qualified Data.Vector as V
import Data.List ( (!?) )
import Data.Maybe (catMaybes)

{-
    Day 03

      Part A
      - given a 2D grid of part numbers and symbols (`.` is a blank)
      - any number adjacent to a symbol is a part number
      - add up all the part numbers
-}


hr :: IO ()
hr = putStrLn $ replicate 42 '-' ++ ['\n']

parseA :: String -> [V.Vector Char]
parseA fileInput = map V.fromList $ lines fileInput

findNum :: V.Vector Char -> ((Maybe Int, Int), V.Vector Char)
findNum vec =
  let maybeIdx = V.findIndex isDigit vec
  in  case maybeIdx of
        Just idx -> let (xs, rest) = V.span isDigit (V.drop idx vec)
                    -- in  ((maybeIdx, (pred . V.length) xs), rest)
                    -- `pred` dropped as V.slice req full length
                    in  ((maybeIdx, V.length xs), rest)
        _        -> ((Nothing, 0), V.empty)

-- check adjSymbol for 114 on line 0
adjSymbol :: (Int, Int) -> Int -> [V.Vector Char] -> Bool
adjSymbol (startIdx, len) lineNum vecs =
  -- ran into indexing issues using V.slice
  -- ... v.drop and V.take "clamp" to valid ranges
  let vec        = vecs !! lineNum
      startIdx'  = max (startIdx - 1) 0
      len'       = if startIdx == 0 then len + 1 else len + 2
      saferSlice = V.take len' . V.drop startIdx'
      -- base version 4.19 req for (!?)
      searchVecs = catMaybes [vecs !? (lineNum - 1), Just vec, vecs !? (lineNum + 1)]
      filterSymbols = filter (/= '.') . filter (not . isDigit)

  -- fmap (* 2) <$> [Just 1, Nothing, Just 3] -- does this help?
  in  not $ null $ filterSymbols $ concat $ map (V.toList . saferSlice) searchVecs

checkAllNumsInLine :: V.Vector Char -> Int -> [V.Vector Char] -> [Int]
checkAllNumsInLine vec lineNum vecs = go [] vec
  where
    go :: [Int] -> (V.Vector Char) -> [Int]
    go acc vec'
      | V.null vec' = acc
      | otherwise   = let ((maybeIdx,len), rest) = findNum vec'
                      in  case maybeIdx of
                            Nothing  -> acc
                            Just idx -> if adjSymbol (idx, len) lineNum vecs
                                          then let numStr = V.toList $ V.slice idx len vec'
                                               in  go (read (numStr) : acc) rest
                                          else go acc rest

partA :: String -> IO ()
partA filePath = do
  fileInput <- readFile filePath
  putStrLn "Day 03 - Part A"

  hr

  let firstFew ss = unlines $ take 10 [show (i, x) | (x, i) <- zip ss [0..]]

  -- I'm thinking of:
  --   1. reading/parsing the input data

  let vecs = parseA fileInput
  putStrLn "First few vectors:"
  putStrLn $ unlines $ take 10 $ map show vecs

  --   2. scaning for a number

  hr

  putStrLn "Find a number (they are still Chars) in a vector:"
  print $ findNum $ head vecs

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
