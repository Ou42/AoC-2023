module Main where

import Data.Char ( isDigit )
import qualified Data.Vector as V
import Data.List ( (!?), foldl' )
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

type SeqStartIdx = Int
type SeqLen      = Int
type LastElem    = Int
type CurrElem    = Int
type Seq         = (SeqStartIdx, SeqLen)

findNums :: V.Vector Char -> [(SeqStartIdx, SeqLen)]
findNums vec
  | null nums = []
  | otherwise = map fst . foldl' go [((head nums, 1), head nums)] $ tail nums
  where
    nums = V.toList $ V.findIndices isDigit vec

    go :: [((SeqStartIdx, SeqLen), LastElem)] -> CurrElem -> [((SeqStartIdx, SeqLen), LastElem)]
    go seqLst@(((seqStartIdx, seqLen), lastElem):rest) currElem
      | (currElem == (lastElem + 1)) = ((seqStartIdx, succ seqLen), currElem):rest
      | (currElem <=  lastElem)      = error $ "ERROR! currElem <= lastElem! INPUT NOT ASCENDING SET!"
                                             ++ "\n" ++ "lastElem = " ++ show lastElem
                                             ++ "\n" ++ "currElem = " ++ show currElem
                                             ++ "\n" ++ "seqLst = " ++ show seqLst
      | otherwise                    = ((currElem, 1), currElem):seqLst

hasAdjSymbol :: (Int, Int) -> Int -> [V.Vector Char] -> Bool
hasAdjSymbol (startIdx, len) lineNum vecs =
  -- ran into indexing issues using V.slice
  -- ... v.drop and V.take "clamp" to valid ranges
  let vec        = vecs !! lineNum
      startIdx'  = max (startIdx - 1) 0
      len'       = if startIdx == 0 then len + 1 else len + 2
      saferSlice = V.take len' . V.drop startIdx'
      -- base version 4.19 req for (!?)
      searchVecs = catMaybes [vecs !? (lineNum - 1), Just vec, vecs !? (lineNum + 1)]
      filterSymbols = filter (/= '.') . filter (not . isDigit)

  in  not $ null $ filterSymbols $ concat $ map (V.toList . saferSlice) searchVecs

checkAllNumsInLine :: Int -> [V.Vector Char] -> [Int]
checkAllNumsInLine lineNum vecs = go vec
  where
    vec    = vecs !! lineNum
    seqLst = findNums vec
    go :: (V.Vector Char) -> [Int]
    go vec'
      | V.null vec' = error "ERROR!! Empty vector!! (checkAllNumsInLine)"
      | otherwise   =
          map (\(idx', len') -> read $ V.toList $ V.slice idx' len' vec')
          $ filter (\(idx, len) -> hasAdjSymbol (idx, len) lineNum vecs) seqLst

checkAllLines :: [V.Vector Char] -> [[Int]]
checkAllLines vecs = map go $ zip vecs [0..]
  where
    go (_, lineNum) = checkAllNumsInLine lineNum vecs

{-
        Find a number (they are still Chars) in a vector:
        ("467..114..",[(5,3),(0,3)])
        ("...*......",[])
        ("..35..633.",[(6,3),(2,2)])
        ("......#...",[])
        ("617*......",[(0,3)])
        (".....+.58.",[(7,2)])
        ("..592.....",[(2,3)])
        ("......755.",[(6,3)])
        ("...$.*....",[])
        (".664.598..",[(5,3),(1,3)])
-}

partA :: String -> IO ()
partA filePath = do
  fileInput <- readFile filePath
  putStrLn "Day 03 - Part A"

  hr

  let firstFew ss = unlines $ take 10 [show (i, x) | (x, i) <- zip ss [0..]]

  -- I'm thinking of:
  --   1. reading/parsing the input data

  let vecs = parseA fileInput
  -- putStrLn "First few vectors:"
  -- putStrLn $ unlines $ take 10 $ map show vecs

  --   2. scan for number strings in each line

  -- hr

  -- putStrLn "Find ALL numbers (they are still Chars) in a vector:"
  -- putStrLn $ unlines $ take 10 $ map show $ zip vecs $ map findNums vecs

  --   3. check all locations around each number string
  --     3a. if a non `.` / non-digit symbol is found, it's a "part number"
  --     3b. goto 3, until the whole 2D grid is processed

  -- hr

  let validNums = checkAllLines vecs
  -- putStrLn "Valid numbers on each line:"
  -- putStrLn $ unlines $ take 10 $ map show $ zip vecs validNums

{-
    Valid numbers on each line:
    ("467..114..",[467])
    ("...*......",[])
    ("..35..633.",[633,35])
    ("......#...",[])
    ("617*......",[617])
    (".....+.58.",[])
    ("..592.....",[592])
    ("......755.",[755])
    ("...$.*....",[])
    (".664.598..",[598,664])
-}

  --   5. sum the list of "part numbers"

  print $ concat $ checkAllLines vecs
  putStr "Part A: Sum of all valid numbers = "
  print $ sum $ concat $ checkAllLines vecs


main :: IO ()
main = do
  -- let filePath = "input-03.test"
  let filePath = "input-03.txt"

  hr

  partA filePath
