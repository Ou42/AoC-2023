module Main where

import qualified Data.List as L
import qualified Data.Set as S
import Data.Set (Set)

{-
    Day 04

      Part A
      - given a list of numbered Cards
      - each card conatins 2 lists:
        - a list of winning numbers
        - a list of numbers we have
      - figure out which of the numbers you have appear in the list of winning numbers
      - Score 1 point for the first match, then double the points for ea match after the 1st
      - What is the total number of points?
-}
type Card = ([Char], (Set Int, Set Int))

hr :: IO ()
hr = putStrLn $ replicate 42 '-' ++ ['\n']

parseDay04 :: String -> [Card]
parseDay04 fileInput =  L.map ( fmap ( go
                                     . fmap (L.drop 1)
                                     . break (== '|')
                                     . L.drop 1 )
                              . break (== ':') 
                              . L.drop 4)
                        $ lines fileInput
  where
    go :: (String, String) -> (Set Int, Set Int)
    go (win, picked) = ( S.fromList $ L.map read $ words win
                       , S.fromList $ L.map read $ words picked )


partA :: [Card] -> [([Char], Int)]
partA = L.map (fmap ((\pow -> 2^(pow-1)) . S.size))
        . L.filter (\(_,s) -> not $ S.null s)
        . L.map (\(cardNo,(win, picked)) -> (cardNo, S.intersection win picked)) 

main :: IO ()
main = do
  fileInput <- readFile "input-04.test"
--   fileInput <- readFile "input-04.txt"

  let parsedData = parseDay04 fileInput
  putStrLn $ unlines $ L.map show parsedData

  hr

--   putStr "The answer for Day 04 - Part A = "
  putStrLn $ unlines $ L.map show $ partA parsedData

{-
        ghci> main
        (" 1",(fromList [17,41,48,83,86],fromList [6,9,17,31,48,53,83,86]))
        (" 2",(fromList [13,16,20,32,61],fromList [17,19,24,30,32,61,68,82]))
        (" 3",(fromList [1,21,44,53,59],fromList [1,14,16,21,63,69,72,82]))
        (" 4",(fromList [41,69,73,84,92],fromList [5,51,54,58,59,76,83,84]))
        (" 5",(fromList [26,28,32,83,87],fromList [12,22,30,36,70,82,88,93]))
        (" 6",(fromList [13,18,31,56,72],fromList [10,11,23,35,36,67,74,77]))

        ------------------------------------------

        (" 1",fromList [17,48,83,86])
        (" 2",fromList [32,61])
        (" 3",fromList [1,21])
        (" 4",fromList [84])
        (" 5",fromList [])
        (" 6",fromList [])

        ------------------------------------------

        (" 1",4)
        (" 2",2)
        (" 3",2)
        (" 4",1)

        ------------------------------------------

        (" 1",8)
        (" 2",2)
        (" 3",2)
        (" 4",1)
-}