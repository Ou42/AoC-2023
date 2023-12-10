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


partA :: [Card] -> Int
partA = undefined

main :: IO ()
main = do
  fileInput <- readFile "input-04.test"
--   fileInput <- readFile "input-04.txt"
  putStrLn $ unlines $ L.map show $ parseDay04 fileInput
